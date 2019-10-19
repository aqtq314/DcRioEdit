namespace DcRioEdit.Models

open System
open System.Collections.Generic
open System.IO
open System.Text

#nowarn "40"
#nowarn "21"


[<AutoOpen>]
module IOUtils =
    let inline tupRev (a, b) = b, a

    let rotByteR count (v : byte) =
        let count = count &&& 7
        byte ((v >>> count) ||| (v <<< (8 - count)))

    let rotByteR2 v =
        rotByteR 2 v

    let rotByteL2 v =
        rotByteR 6 v

    type BinaryReader with
        member x.ReadFixedSizeString length =
            let bytes = x.ReadBytes length
            let str = shiftJis.GetString bytes
            str.TrimEnd '\000'

    type BinaryWriter with
        member x.WriteFixedSizeString length text =
            let bytes = shiftJis.GetBytes (text : string)
            let bytesPadded =
                if bytes.Length < length then
                    Array.append bytes (Array.zeroCreate (length - bytes.Length))
                elif bytes.Length > length then
                    Array.sub bytes 0 length
                else
                    bytes
            x.Write bytesPadded


type Serializer<'a> (getByteCount, read, write) =
    member x.GetByteCount (value : 'a) = getByteCount value
    member x.Read (reader : BinaryReader) : 'a = read reader
    member x.Write (writer : BinaryWriter) (content : 'a) : unit = write writer content

type Serializer =
    static member inline create byteCount read write =
        Serializer<_> (byteCount, read, write)

    static member string byteCount =
        Serializer.create
            (fun _ -> byteCount)
            (fun reader -> reader.ReadFixedSizeString byteCount)
            (fun writer content -> writer.WriteFixedSizeString byteCount content)

    static member int =
        Serializer.create
            (fun _ -> 4)
            (fun reader -> reader.ReadInt32 ())
            (fun writer content -> writer.Write content)

    static member rotatedByteArray readByteCount =
        Serializer.create
            Array.length
            (fun reader -> Array.map rotByteR2 (reader.ReadBytes readByteCount))
            (fun writer content -> writer.Write (Array.map rotByteL2 content))


type ICellBase<'a> =
    abstract Value : 'a
    abstract ByteCount : int
    abstract Write : writer : BinaryWriter -> unit

type ICell<'a> =
    inherit ICellBase<'a>
    abstract SetValue : newValue : 'a -> unit

module Cell =
    let create (serializer : Serializer<_>) value =
        let mutable value = value
        { new ICell<_> with
            member a.Value = value
            member a.SetValue newValue = value <- newValue
            member a.ByteCount = serializer.GetByteCount value
            member a.Write writer = serializer.Write writer value }

    let createRead (serializer : Serializer<_>) reader =
        create serializer (serializer.Read reader)

    let map (serializer : Serializer<_>) mapper (cell : #ICellBase<_>) =
        { new ICellBase<_> with
            member a.Value = mapper cell.Value
            member a.ByteCount = serializer.GetByteCount a.Value
            member a.Write writer = serializer.Write writer a.Value }


type ScriptFile (filename, offset, content) =
    let size = Cell.map Serializer.int Array.length content
    member x.FileName = filename
    member x.Size = size
    member x.Offset = offset
    member x.Content = content

    static member read reader =
        let filename = Cell.createRead (Serializer.string 13) reader :> ICellBase<_>
        let size = Serializer.int.Read reader
        let offset = Cell.createRead Serializer.int reader
        fun reader ->
            let content = Cell.createRead (Serializer.rotatedByteArray size) reader
            ScriptFile (filename, offset, content)

    static member write writer (x : ScriptFile) =
        x.FileName.Write writer
        x.Size.Write writer
        x.Offset.Write writer
        fun writer ->
            x.Content.Write writer

    static member updateOffset offset (x : ScriptFile) =
        offset +
        x.FileName.ByteCount +
        x.Size.ByteCount +
        x.Offset.ByteCount,
        fun offset ->
            x.Offset.SetValue offset
            offset + x.Content.ByteCount


type ArchiveFormat (extension, offset, files) =
    let count = Cell.create Serializer.int (Array.length files) :> ICellBase<_>
    member x.Extension = extension
    member x.Count = count
    member x.Offset = offset
    member x.Files = files

    static member read reader =
        let extension = Cell.createRead (Serializer.string 4) reader :> ICellBase<_>
        let count = Serializer.int.Read reader
        let offset = Cell.createRead Serializer.int reader
        fun reader ->
            let ffiles = Array.init count (fun _ -> ScriptFile.read reader)
            fun reader ->
                let files = ffiles |> Array.map (fun ffile -> ffile reader)
                ArchiveFormat (extension, offset, files)

    static member write writer (x : ArchiveFormat) =
        x.Extension.Write writer
        x.Count.Write writer
        x.Offset.Write writer
        fun writer ->
            let wfiles = Array.map (ScriptFile.write writer) x.Files
            fun writer ->
                wfiles |> Array.iter ((|>) writer)

    static member updateOffset offset (x : ArchiveFormat) =
        offset +
        x.Extension.ByteCount +
        x.Count.ByteCount +
        x.Offset.ByteCount,
        fun offset ->
            x.Offset.SetValue offset
            let fileUpdateOffsets, offset = Array.mapFold (fun offset file -> ScriptFile.updateOffset offset file |> tupRev) offset x.Files
            offset,
            fun offset ->
                Array.fold (fun offset fileUpdateOffset -> fileUpdateOffset offset) offset fileUpdateOffsets


type Archive (formats) =
    let count = Cell.create Serializer.int (Array.length formats) :> ICellBase<_>
    member x.Count = count
    member x.Formats = formats

    static member read reader =
        let count = Serializer.int.Read reader
        let formats =
            Array.init count (fun _ -> ArchiveFormat.read reader)
            |> Array.map ((|>) reader)
            |> Array.map ((|>) reader)
        Archive (formats)

    static member write writer (x : Archive) =
        x.Count.Write writer
        Array.map (ArchiveFormat.write writer) x.Formats
        |> Array.map ((|>) writer)
        |> Array.iter ((|>) writer)

    static member updateOffset (x : Archive) =
        let offset =
            x.Count.ByteCount
        let formatUpdateOffsets, offset = Array.mapFold (fun offset format -> ArchiveFormat.updateOffset offset format |> tupRev) offset x.Formats
        let formatUpdateOffsets, offset = Array.mapFold (fun offset formatUpdateOffset -> formatUpdateOffset offset |> tupRev) offset formatUpdateOffsets
        let offset = Array.fold (fun offset formatUpdateOffset -> formatUpdateOffset offset) offset formatUpdateOffsets
        ()


module ArchiveIO =
    let load filePath =
        use file = File.OpenRead filePath
        use reader = new BinaryReader (file, shiftJis, true)
        Archive.read reader

    let save outPath archive =
        Archive.updateOffset archive
        use newfile = File.Create outPath
        use writer = new BinaryWriter (newfile, shiftJis, true)
        Archive.write writer archive


