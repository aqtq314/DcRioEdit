﻿namespace DcRioEdit.Models

open FSharp.Data
open Microsoft.Win32
open System
open System.Collections.Generic
open System.Text
open System.Windows
open System.Windows.Data

#nowarn "40"
#nowarn "21"


type TranslationCsv = CsvProvider<Schema = @"ScriptName (string), Start (int), End (int), TranslationText (string)", HasHeaders = false>

type TranscriptionContext<'a> =
    abstract GetMinStart : item : 'a -> int
    abstract GetMaxEnd : item : 'a -> int
    abstract Transcribe : range : IntRange -> string
    abstract RemoveTranscription : item : 'a -> unit

type Transcription (getContext, range, text, translation) =
    let range, rangeW = stwpfp range
    let rangeText =
        range
        |> St.map (fun (range : IntRange) ->
            sprintf "[0x%X, 0x%X)" range.Start range.End)

    let text =
        range
        |> St.mapd text (fun range ->
            let context : TranscriptionContext<_> = getContext ()
            context.Transcribe range)

    let translation, translationW = stwpfp translation

    member x.Range = range
    member x.RangeW = rangeW
    member x.RangeText = rangeText
    member x.Text = text
    member x.Translation = translation
    member x.TranslationW = translationW

    member x.RemoveSelf () =
        let context = getContext ()
        context.RemoveTranscription x

    member x.RemoveSelfBehavior =
        let rec click = behavior {
            let! (e : BehaviorClickEventArgs) = ()
            x.RemoveSelf ()
            return! click }

        Behavior.clickBehavior click

    interface ITranscription with
        member x.Range
            with get () = !!x.Range
            and set value = x.Range |> St.set value


type ScriptFileViewModel (fileModel : ScriptFile) as x =
    let fileName = fileModel.FileName.Value
    let content = ArraySeg.ofArrayUnchecked fileModel.Content.Value

    let scrollOffset, scrollOffsetW = stwpfp 0

    let rec buildContext () =
        { new TranscriptionContext<_> with
            member a.GetMinStart item = x.GetTranscriptionMinStart item
            member a.GetMaxEnd item = x.GetTranscriptionMaxEnd item
            member a.Transcribe range = x.TranscribeText range
            member a.RemoveTranscription item = transcriptions.ForceRemove item }

    and transcriptions : _ collectionState =
        stc (ScriptFileViewModel.detectTranscriptions content
        |> Seq.map (fun range ->
            let text = ScriptFileViewModel.transcribeText content range
            let translation = text
            Transcription (buildContext, range, text, translation)))

    static member transcribeText content (range : IntRange) =
        shiftJis.GetString (ArraySeg.toArray (ArraySeg.sub range.Start range.Length content))

    member x.ScrollOffset = scrollOffset
    member x.ScrollOffsetW = scrollOffsetW

    member x.FileName : string = fileName
    member x.Content : ArraySeg<byte> = content
    member x.ContentLengthString =
        sprintf "%0.2f KB" (float x.Content.Length / 1024.0)

    member x.Transcriptions = transcriptions

    member x.TranscribeText range =
        ScriptFileViewModel.transcribeText x.Content range

    member x.LoadTranslations translations =
        seq { x.Transcriptions.Count - 1 .. -1 .. 0 }
        |> Seq.iter x.Transcriptions.RemoveAt

        translations
        |> Seq.map(fun (row : TranslationCsv.Row) ->
            let range = IntRange.ofStartEnd(row.Start, row.End)
            let transcriptionText = x.TranscribeText range
            Transcription(buildContext, range, transcriptionText, row.TranslationText))
        |> Seq.iter x.Transcriptions.Add

    member x.GetTranscriptionMinStart (item : Transcription) =
        let index = transcriptions.IndexOf item
        if index <= 0 then 0
        else transcriptions.[index - 1].Range.Value.End

    member x.GetTranscriptionMaxEnd (item : Transcription) =
        let index = transcriptions.IndexOf item
        if index >= transcriptions.Count - 1 then x.Content.Length
        else transcriptions.[index + 1].Range.Value.Start

    member x.GetTranscriptionMaxRange (item : Transcription) =
            let index = x.Transcriptions.IndexOf item
            let minStart =
                if index = 0 then 0
                else x.Transcriptions.[index - 1].Range.Value.End
            let maxEnd =
                if index = x.Transcriptions.Count - 1 then x.Content.Length
                else x.Transcriptions.[index + 1].Range.Value.Start
            IntRange.ofStartEnd (minStart, maxEnd)

    static member detectTranscriptions (content : ArraySeg<byte>) =
        seq {
            let layout =
                let charByteLayout = Array.map CharByteLayout.get (ArraySeg.toArray content)
                for i in 0 .. charByteLayout.Length - 1 do
                    match charByteLayout.[i] with
                    | PrintDouble when i + 1 < charByteLayout.Length ->
                        charByteLayout.[i + 1] <- Unprinted
                    | PrintDouble ->
                        charByteLayout.[i] <- PrintSingle
                    | _ ->
                        ()
                charByteLayout

            let mutable rangeStart = None
            let mutable i = 0
            let iterCount = content.Length - 1
            while i < iterCount do
                let includeByte, advance =
                    if (layout.[i] = PrintDouble &&
                        (shiftJis.GetChars [| content.[i]; content.[i + 1] |] <> [| '・' |] ||
                            (content.[i] = 0x81uy && content.[i + 1] = 0x45uy))) then
                        true, 2
                    elif content.[i] = byte '\\' && content.[i + 1] = byte 'n' then
                        true, 2
                    elif content.[i] = byte '{' || content.[i] = byte ':' || content.[i] = byte '}' || content.[i] = byte ' ' then
                        true, 1
                    else
                        false, 1
                match rangeStart, includeByte with
                | None, true ->
                    rangeStart <- Some i
                    i <- i + advance
                | None, false ->
                    i <- i + advance
                | Some rangeStartValue, true ->
                    i <- i + advance
                | Some rangeStartValue, false ->
                    if i - rangeStartValue > 2 then
                        yield IntRange.ofStartEnd (rangeStartValue, i)
                    rangeStart <- None
                    i <- i + advance
            match rangeStart with
            | None -> ()
            | Some rangeStartValue ->
                yield IntRange.ofStartEnd (rangeStartValue, i) }

    member x.UpdateModel() =
        let newContent = Array.concat(seq {
            let content = x.Content |> ArraySeg.toArray
            let mutable prevEnd = 0
            for tr in x.Transcriptions do
                let range = !!tr.Range
                if prevEnd < range.Start then
                    yield Array.sub content prevEnd (range.Start - prevEnd)
                yield gb2312.GetBytes !!tr.Translation
                prevEnd <- range.End
            if prevEnd < content.Length then
                yield Array.sub content prevEnd (content.Length - prevEnd) })

        printfn "updateModel [%s]: %A" x.FileName (fileModel.Content.Value = newContent)

        fileModel.Content.SetValue newContent

    interface IScriptFile with
        member x.Content = x.Content
        member x.Item with get index = x.Transcriptions.[index] :> _
        member x.Count = x.Transcriptions.Count

        member x.InsertNew index range =
            let item = Transcription (buildContext, range, x.TranscribeText range, "")
            x.Transcriptions.Insert index item
            item :> _

        member x.Remove item =
            x.Transcriptions.ForceRemove (item :?> Transcription)

        member x.RemoveAt index =
            x.Transcriptions.RemoveAt index

        member x.GetMaxRange item =
            x.GetTranscriptionMaxRange (item :?> Transcription)

    interface IEnumerable<ITranscription> with
        member x.GetEnumerator () =
            (x.Transcriptions |> Seq.map (fun transcription -> transcription :> ITranscription)).GetEnumerator ()

    interface System.Collections.IEnumerable with
        member x.GetEnumerator () =
            (x.Transcriptions :> System.Collections.IEnumerable).GetEnumerator ()


type FormatViewModel (formatModel : ArchiveFormat) =
    let extension = formatModel.Extension.Value
    let files = ArraySeg.mapOfArray ScriptFileViewModel formatModel.Files

    member x.Extension : string = extension
    member x.Files : ArraySeg<ScriptFileViewModel> = files


type ArchiveViewModel (filePath, archiveModel : Archive) =
    let formats = ArraySeg.mapOfArray FormatViewModel archiveModel.Formats

    let files =
        formats
        |> ArraySeg.collect (fun (format : FormatViewModel) ->
            format.Files)

    let currFile = st WpfNone

    let filesViewSource = CollectionViewSource.GetDefaultView files
    do filesViewSource.CurrentChanged.Add (fun e ->
        let newCurrFile =
            match filesViewSource.CurrentItem with
            | null -> WpfNone
            | :? ScriptFileViewModel as file -> WpfSome file
            | _ -> WpfNone
        currFile |> St.set newCurrFile)

    do  let index = files |> Seq.tryFindIndex (fun file -> file.Transcriptions.Count > 0)
        Option.iter (filesViewSource.MoveCurrentToPosition >> ignore) index

    member x.FilePath : string = filePath
    member x.Formats : ArraySeg<FormatViewModel> = formats
    member x.Files = files
    member x.FilesViewSource = filesViewSource
    member x.CurrentFile = currFile

    member x.FileName = IO.Path.GetFileName x.FilePath

    member x.LoadTranslation (fileName : string) =
        use csvStream = new IO.StreamReader(fileName, gb2312)
        let csv = TranslationCsv.Load csvStream
        let rows = Array.ofSeq csv.Rows
        let rowsByFile = rows |> Array.groupBy(fun row -> row.ScriptName)
        
        let fileVmMap = x.Files |> Seq.map(fun file -> file.FileName, file) |> Map.ofSeq
        rowsByFile |> Array.iter(fun (fileName, rows) ->
            rows |> fileVmMap.[fileName].LoadTranslations)

    member x.LoadTranslationBehavior =
        let rec click = behavior {
            let! (e : BehaviorClickEventArgs) = ()

            let dialog = OpenFileDialog ()
            dialog.Filter <- "CSV 文件|*.csv"
            dialog.FileName <- "translation.csv"
            let showDialogResult = dialog.ShowDialog ()
            if showDialogResult.Value then
                do  try x.LoadTranslation dialog.FileName
                    with
                    | ex ->
                        MessageBox.Show(
                            ex.Message + Environment.NewLine + ex.StackTrace,
                            "错误", MessageBoxButton.OK, MessageBoxImage.Error) |> ignore

            return! click }

        Behavior.clickBehavior click

    member x.SaveTranslation (fileName : string) =
        let csv =
            new TranslationCsv(
                x.Files |> Seq.collect(fun file ->
                    file.Transcriptions |> Seq.map(fun transcription ->
                        let range = !!transcription.Range
                        TranslationCsv.Row(file.FileName, range.Start, range.End, !!transcription.Translation))))
        let outString = csv.SaveToString()
        IO.File.WriteAllText(fileName, outString, gb2312)

    member x.SaveTranslationBehavior =
        let rec click = behavior {
            let! (e : BehaviorClickEventArgs) = ()

            let dialog = SaveFileDialog ()
            dialog.Filter <- "CSV 文件|*.csv"
            dialog.FileName <- "translation.csv"
            let showDialogResult = dialog.ShowDialog ()
            if showDialogResult.Value then
                do  try x.SaveTranslation dialog.FileName
                        System.Media.SystemSounds.Beep.Play()
                    with
                    | ex ->
                        MessageBox.Show(
                            ex.Message + Environment.NewLine + ex.StackTrace,
                            "错误", MessageBoxButton.OK, MessageBoxImage.Error) |> ignore

            return! click }

        Behavior.clickBehavior click

    member x.UpdateModel() =
        x.Files |> ArraySeg.iter(fun file ->
            file.UpdateModel())

    member x.Save arcPath =
        x.UpdateModel()
        ArchiveIO.save arcPath archiveModel

    member x.SaveBehavior =
        let rec click = behavior {
            let! (e : BehaviorClickEventArgs) = ()

            let dialog = SaveFileDialog ()
            dialog.Filter <- "Rio.arc|Rio.arc"
            dialog.FileName <- "Rio.arc"
            let showDialogResult = dialog.ShowDialog ()
            if showDialogResult.Value then
                do  try x.Save dialog.FileName
                        System.Media.SystemSounds.Beep.Play()
                    with
                    | ex ->
                        MessageBox.Show(
                            ex.Message + Environment.NewLine + ex.StackTrace,
                            "错误", MessageBoxButton.OK, MessageBoxImage.Error) |> ignore

            return! click }

        Behavior.clickBehavior click


type MasterViewModel () =
    let archive = st WpfNone

    member val Window : Window = null with get, set

    member x.Archive = archive

    member x.OpenArc filePath =
        let archiveModel = ArchiveIO.load filePath
        let newArchive = ArchiveViewModel (filePath, archiveModel)
        archive.SetValue (WpfSome newArchive)

    member x.OpenArcBehavior =
        let rec click = behavior {
            let! (e : BehaviorClickEventArgs) = ()

            let dialog = OpenFileDialog ()
            dialog.Filter <- "Rio.jp.arc|Rio.jp.arc;Rio.arc"
            dialog.FileName <- "Rio.jp.arc"
            let showDialogResult = dialog.ShowDialog x.Window
            if showDialogResult.Value then
                let bakFileName =
                    if (IO.Path.GetFileName dialog.FileName).Equals("Rio.arc", StringComparison.CurrentCultureIgnoreCase) then
                        let msgboxResult = MessageBox.Show ("您即将打开 Rio.arc 素材库本体，本程序将为您复制一份备份文件至 Rio.jp.arc。如果 Rio.jp.arc 已存在，将会被覆盖掉。", "素材库文件备份确认", MessageBoxButton.OKCancel, MessageBoxImage.Exclamation)
                        if msgboxResult = MessageBoxResult.OK then
                            let bakFileName = IO.Path.Combine (IO.Path.GetDirectoryName dialog.FileName, "Rio.jp.arc")
                            IO.File.Copy (dialog.FileName, bakFileName, true)
                            bakFileName
                        else
                            null
                    else
                        dialog.FileName

                if not (String.IsNullOrEmpty bakFileName) then
                    x.OpenArc bakFileName

            return! click }

        Behavior.clickBehavior click


//module DesignerViewModels =
//    let scriptFile =
//        let bytes =
//            System.IO.File.ReadAllBytes @"E:\Program Files\DYNAMIC CHORD\DYNAMIC CHORD feat.[reve parfait] Append Disc\Data\EAKI_01.WSC"
//            |> Array.map rotByteR2
//            |> ArraySeg.ofArrayUnchecked
//
//        ScriptFileViewModel ("EAKI_01", bytes, Seq.empty)
//
//    let masterViewModel =
//        try
//            MessageBox.Show "Loading" |> ignore
//            let vm = MasterViewModel ()
//            vm.OpenFile @"E:\Program Files\DYNAMIC CHORD\DYNAMIC CHORD feat.[reve parfait] Append Disc\Rio.arc"
//            let archive = vm.Archive.Value.Value
//            archive.FilesViewSource.MoveCurrentTo archive.Files.[2] |> ignore
//        withvm
//        | ex ->
//            let asm = typeof<SkiaSharp.SKPaint>.Assembly
//            MessageBox.Show (sprintf "%s\r\n%s" asm.Location asm.CodeBase) |> ignore
//            //MessageBox.Show (ex.ToString ()) |> ignore
//            Unchecked.defaultof<_>
//
//    let archiveViewModel =
//        masterViewModel.Archive.Value.Value


