namespace DcRioEdit.Models

open System
open System.Collections.Generic
open System.Text

#nowarn "40"
#nowarn "21"


[<AutoOpen>]
module ViewModelUtils =
    let shiftJis = Encoding.GetEncoding 932
    let gb2312 = Encoding.GetEncoding 936


type TranscriptionViewModelContext<'a> =
    abstract GetMinStart : item : 'a -> int
    abstract GetMaxEnd : item : 'a -> int
    abstract Transcribe : range : Range -> string
    abstract RemoveTranscription : item : 'a -> unit

type TranscriptionViewModel (getContext, range, text, translation) =
    let range, rangeW = stwpfp range

    let text =
        range
        |> St.mapd text (fun range ->
            let context : TranscriptionViewModelContext<_> = getContext ()
            context.Transcribe range)

    let translation, translationW = stwpfp translation

    member x.Range = range
    member x.RangeW = rangeW
    member x.Text = text
    member x.Translation = translation
    member x.TranslationW = translationW

    member x.RemoveSelf () =
        let context = getContext ()
        context.RemoveTranscription x




type FileViewModel (fileName, extension, content, buildTranscriptions) as x =
    let rec transcriptions : _ collectionState =
        let buildContext () =
            { new TranscriptionViewModelContext<_> with
                member a.GetMinStart item = x.GetTranscriptionMinStart item
                member a.GetMaxEnd item = x.GetTranscriptionMaxEnd item
                member a.Transcribe range = x.TranscribeText range
                member a.RemoveTranscription item = transcriptions.ForceRemove item }
        stc (Seq.map (fun builder -> builder buildContext) buildTranscriptions)

    member x.FileName : string = fileName
    member x.Extension : string = extension
    member x.Content : ArraySeg<byte> = content

    member x.FullFileName = sprintf "%s.%s" fileName extension

    member x.Transcriptions = transcriptions

    member x.TranscribeText (range : Range) =
        shiftJis.GetString (ArraySeg.toArray (ArraySeg.sub range.Start range.Length content))

    member x.GetTranscriptionMinStart (item : TranscriptionViewModel) =
        let index = transcriptions.IndexOf item
        if index <= 0 then 0
        else transcriptions.[index - 1].Range.Value.End

    member x.GetTranscriptionMaxEnd (item : TranscriptionViewModel) =
        let index = transcriptions.IndexOf item
        if index >= transcriptions.Count - 1 then x.Content.Length
        else transcriptions.[index + 1].Range.Value.Start


type FormatViewModel (extension, files) =
    member x.Extension : string = extension
    member x.Files : ArraySeg<FileViewModel> = files


type ArchiveViewModel (fileName, formats) =
    let files = formats |> ArraySeg.collect (fun (format : FormatViewModel) ->
        format.Files)
    member x.FileName : string = fileName
    member x.Formats : ArraySeg<FormatViewModel> = formats


module DesignerViewModels =
    let rotByteR count (v : byte) =
        let count = count &&& 7
        byte ((v >>> count) ||| (v <<< (8 - count)))

    let rotByteR2 v =
        rotByteR 2 v

    let testByteList =
        System.IO.File.ReadAllBytes @"E:\Program Files\DYNAMIC CHORD\DYNAMIC CHORD feat.[reve parfait] Append Disc\Data\SC\RAKI_16.SC"
        |> Array.map rotByteR2


