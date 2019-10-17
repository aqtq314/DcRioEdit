namespace DcRioEdit.Models

open SkiaSharp
open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Threading.Tasks
open System.Windows
open System.Windows.Controls
open System.Windows.Controls.Primitives
open System.Windows.Data
open System.Windows.Documents
open System.Windows.Input
open System.Windows.Interop
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Navigation
open System.Windows.Shapes


type CharByteLayout =
    | Unprinted
    | PrintSingle
    | PrintDouble

    static member get charByte =
        if charByte = 0x80uy || charByte = 0xA0uy then Unprinted
        elif charByte <= 0x7Fuy || (charByte >= 0xA0uy && charByte <= 0xDFuy) || charByte >= 0xF0uy then PrintSingle
        else PrintDouble


[<AutoOpen>]
module SkiaUtils =
    type SKCanvas with
        member x.SaveLoadCurrentTransform () =
            let currMatrix = x.TotalMatrix
            Disposable.create (fun () ->
                x.SetMatrix currMatrix)

    type SKColor with
        static member ofIntValue intValue =
            SKColor (
                byte (intValue >>> 16),
                byte (intValue >>> 8),
                byte (intValue >>> 0),
                byte (intValue >>> 24))


module ByteTileSheet =
    let controlChars = [| yield! seq { 0uy .. 0x1Fuy }; yield 0x7Fuy |]
    let asciiChars = [| 0x20uy .. 0x7Euy |]
    let kanjiChars = [| 0xA1uy .. 0xDFuy |]
    let unusedChars = [| yield 0x80uy; yield 0xA0uy; yield! seq { 0xF0uy .. 0xFFuy } |]

    let controlCharNames = [| "NUL"; "SOH"; "STX"; "ETX"; "EOT"; "ENQ"; "ACK"; "BEL"; "BS"; "HT"; "NL"; "VT"; "NP"; "CR"; "SO"; "SI"; "DLE"; "DC1"; "DC2"; "DC3"; "DC4"; "NAK"; "SYN"; "ETB"; "CAN"; "EM"; "SUB"; "ESC"; "FS"; "GS"; "RS"; "US"; "DEL" |]

    let charWidth = 10
    let charHeight = 22
    let byteCodeWidth = 18
    let rowNumberWidth = 54

    let asciiVOffset = 17
    let kanjiVOffset = 18
    let unusedVOffset = 14
    let byteCodeVOffset = 16
    let rowNumberVOffset = 16

    let charCount = 256
    let rowCharCount = 32

    let getCharDispOffset rowCharCount charIndex =
        let x = (charIndex % rowCharCount) * charWidth
        let y = charIndex / rowCharCount * charHeight
        SKPointI (x, y)

    let getCharDrawOffset rowCharCount vOffset isDoubleByte charIndex =
        let offset = getCharDispOffset rowCharCount (int charIndex)
        let charXOffset = if isDoubleByte then charWidth else charWidth / 2
        SKPoint (float32 (offset.X + charXOffset), float32 (offset.Y + vOffset))

    let getByteDispOffset rowCharCount charIndex =
        let x = (charIndex % rowCharCount) * byteCodeWidth
        let y = charIndex / rowCharCount * charHeight
        SKPointI (x, y)

    let getByteDrawOffset rowCharCount charIndex =
        let offset = getByteDispOffset rowCharCount charIndex
        SKPoint (float32 (offset.X + byteCodeWidth / 2), float32 (offset.Y + byteCodeVOffset))

    let controlPaint, asciiPaint, kanjiPaint, unusedPaint, byteCodePaint, rowNumberPaint =
        let makePaint (darkness, fontName, fontSize, fontWeight, textAlign, smoothText) =
            let paint = new SKPaint ()
            paint.Color <- SKColor (darkness, darkness, darkness)
            paint.Typeface <- SKTypeface.FromFamilyName (fontName, fontWeight, SKFontStyleWidth.Normal, SKFontStyleSlant.Upright)
            paint.TextSize <- fontSize
            paint.TextAlign <- textAlign
            paint.IsAntialias <- true
            paint.SubpixelText <- smoothText
            paint.LcdRenderText <- smoothText
            paint
        makePaint (0xC0uy, "PF Tempesta Five", 8.0f, SKFontStyleWeight.Normal, SKTextAlign.Center, false),
        makePaint (0x60uy, "Source Code Pro", 15.0f, SKFontStyleWeight.Normal, SKTextAlign.Center, true),
        makePaint (0uy, "NfMotoya Aporo Std W1", 18.0f, SKFontStyleWeight.Normal, SKTextAlign.Center, true),
        makePaint (0xC0uy, "Source Code Pro", 12.0f, SKFontStyleWeight.Normal, SKTextAlign.Center, false),
        makePaint (0uy, "M+ 1m", 13.5f, SKFontStyleWeight.Normal, SKTextAlign.Center, true),
        makePaint (0x80uy, "M+ 1m", 13.5f, SKFontStyleWeight.Normal, SKTextAlign.Left, true)

    let makePaint color style blendMode =
        let paint = new SKPaint ()
        paint.Color <- color
        paint.Style <- style
        paint.IsAntialias <- true
        paint.StrokeWidth <- 1.0f
        paint.PathEffect <- SKPathEffect.CreateCorner (3.0f)
        paint.BlendMode <- blendMode
        paint
    let selectionTintPaint   = makePaint (SKColor.ofIntValue 0xFF420763) SKPaintStyle.Fill SKBlendMode.Screen
    let selectionBorderPaint = makePaint (SKColor.ofIntValue 0xFF6F2995) SKPaintStyle.Stroke SKBlendMode.Multiply
    let selectionFillPaint   = makePaint (SKColor.ofIntValue 0xFFE4CCF1) SKPaintStyle.Fill SKBlendMode.Multiply
    let deletionBorderPaint = makePaint (SKColor.ofIntValue 0xFFDE3535) SKPaintStyle.Stroke SKBlendMode.Src
    let mouseOverPaint = makePaint (SKColor.ofIntValue 0xFF9BD031) SKPaintStyle.Stroke SKBlendMode.Src
    let mouseDownPaint = makePaint (SKColor.ofIntValue 0xFFE4CCF1) SKPaintStyle.Fill SKBlendMode.Multiply

    let charTileBitmap =
        let bitmap = new SKBitmap (charWidth * rowCharCount, charCount / rowCharCount * charHeight, SKColorType.Bgra8888, SKAlphaType.Premul)
        let bitmapPtr, bitmapLength = bitmap.GetPixels ()

        bitmap.LockPixels ()
        use surface = SKSurface.Create (bitmap.Width, bitmap.Height, SKColorType.Bgra8888, SKAlphaType.Premul, bitmapPtr, bitmap.RowBytes)
        let canvas = surface.Canvas
        canvas.Clear (SKColor (0xFFuy, 0xFFuy, 0xFFuy, 0xFFuy))

        let controlCharOffsets =
            (controlChars, controlCharNames)
            ||> Seq.map2 (fun charByte charName ->
                Array.init charName.Length (fun i ->
                    let offset = getCharDrawOffset rowCharCount 0 false (int charByte)
                    SKPoint (offset.X, offset.Y + float32 (i * 6 + 8))))
            |> Array.concat
        canvas.DrawPositionedText (
            String.concat "" controlCharNames,
            controlCharOffsets,
            controlPaint)

        canvas.DrawPositionedText (
            shiftJis.GetString asciiChars,
            [| for i in asciiChars -> getCharDrawOffset rowCharCount asciiVOffset false (int i) |],
            asciiPaint)

        canvas.DrawPositionedText (
            shiftJis.GetString kanjiChars,
            [| for i in kanjiChars -> getCharDrawOffset rowCharCount kanjiVOffset false (int i) |],
            kanjiPaint)

        canvas.DrawPositionedText (
            String.replicate unusedChars.Length "·",
            [| for i in unusedChars -> getCharDrawOffset rowCharCount unusedVOffset false (int i) |],
            unusedPaint)

        bitmap.UnlockPixels ()
        bitmap

    let byteTileBitmap =
        let bitmap = new SKBitmap (byteCodeWidth * rowCharCount, charCount / rowCharCount * charHeight, SKColorType.Bgra8888, SKAlphaType.Premul)
        let bitmapPtr, bitmapLength = bitmap.GetPixels ()

        bitmap.LockPixels ()
        use surface = SKSurface.Create (bitmap.Width, bitmap.Height, SKColorType.Bgra8888, SKAlphaType.Premul, bitmapPtr, bitmap.RowBytes)
        let canvas = surface.Canvas
        canvas.Clear (SKColor (0xFFuy, 0xFFuy, 0xFFuy))

        for i in 0 .. charCount - 1 do
            let offset = getByteDrawOffset rowCharCount i
            canvas.DrawText (
                i.ToString "X2",
                offset.X,
                offset.Y,
                byteCodePaint)

        bitmap.UnlockPixels ()
        bitmap


open ByteTileSheet

type BitmapBuffer (width, height) =
    let tileBitmap = new SKBitmap (width, height, SKColorType.Bgra8888, SKAlphaType.Premul)
    let dispBitmap = WriteableBitmap (width, height, 96.0, 96.0, PixelFormats.Pbgra32, null)

    member x.Width = width
    member x.Height = height

    member x.TileBitmap = tileBitmap
    member x.DispBitmap = dispBitmap

    member x.DrawTile draw =
        let bitmapPtr, bitmapLength = tileBitmap.GetPixels ()
        tileBitmap.LockPixels ()
        do  use surface = SKSurface.Create (width, height, SKColorType.Bgra8888, SKAlphaType.Premul, bitmapPtr, tileBitmap.RowBytes)
            draw surface

        tileBitmap.UnlockPixels ()

    member x.DrawDisp draw =
        dispBitmap.Lock ()
        do  use surface = SKSurface.Create (width, height, SKColorType.Bgra8888, SKAlphaType.Premul, dispBitmap.BackBuffer, dispBitmap.BackBufferStride)
            draw surface

        dispBitmap.AddDirtyRect (Int32Rect (0, 0, width, height))
        dispBitmap.Unlock ()


type ByteTileMouseOverStatus =
    | OverSingleByte of index : int
    | OverDoubleByte of firstIndex : int

    member x.AsRange =
        match x with
        | OverSingleByte index -> Range.unitLength index
        | OverDoubleByte firstIndex -> Range.ofLength firstIndex 2


type ByteTileMouseDownButton<'l, 'r> =
    | LeftButton of data : 'l
    | RightButton of data : 'r

type ByteTileMouseStatus<'a, 'b> =
    | MouseReleased of data : 'a
    | MousePressed of data : 'b

type ByteTileColumn =
    | ByteColumn
    | CharColumn


type ByteTile () =
    inherit FrameworkElement ()

    static let columnSpacing = 16

    let mutable charByteLayout = Array.empty

    let mutable redrawTileBuffer = true
    let mutable redrawDispBuffer = true
    let mutable buffer : BitmapBuffer option = None

    let mutable hviewport = 0.0
    let mutable vviewport = 0
    let mutable hoffset = 0.0

    let mutable mouseStatus = MouseReleased None

    static member buildCharByteLayout (bytes : ArraySeg<byte>) =
        let charByteLayout = Array.init bytes.Length (fun i -> CharByteLayout.get bytes.[i])
        for i in 0 .. charByteLayout.Length - 1 do
            match charByteLayout.[i] with
            | PrintDouble when i + 1 < charByteLayout.Length ->
                charByteLayout.[i + 1] <- Unprinted
            | PrintDouble ->
                charByteLayout.[i] <- PrintSingle
            | _ ->
                ()
        charByteLayout

    static member val ScriptFileProperty =
        Dp.reg<IScriptFile, ByteTile> "ScriptFile"
            (Dp.Meta (ScriptFile.empty, Dp.MetaFlags.AffectsArrange ||| Dp.MetaFlags.AffectsRender, fun o (oldValue, newValue) -> (o : ByteTile).OnScriptFileChanged (oldValue, newValue)))

    member x.ScriptFile
        with get () = x.GetValue ByteTile.ScriptFileProperty :?> IScriptFile
        and set v = x.SetValue (ByteTile.ScriptFileProperty, box (v : IScriptFile))

    member x.OnScriptFileChanged (oldValue, newValue) =
        charByteLayout <- ByteTile.buildCharByteLayout newValue.Content
        x.ScrollOwner |> Option.iter (fun owner -> owner.InvalidateScrollInfo ())
        redrawTileBuffer <- true

    member x.Bytes = x.ScriptFile.Content

    static member val CharsPerRowProperty =
        Dp.reg<int, ByteTile> "CharsPerRow"
            (Dp.Meta (32, Dp.MetaFlags.AffectsRender, fun o (oldValue, newValue) -> (o : ByteTile).OnCharsPerRowChanged (oldValue, newValue)))

    member x.CharsPerRow
        with get () = x.GetValue ByteTile.CharsPerRowProperty :?> int
        and set v = x.SetValue (ByteTile.CharsPerRowProperty, box (v : int))

    member x.OnCharsPerRowChanged (oldValue, newValue) =
        x.ScrollOwner |> Option.iter (fun owner -> owner.InvalidateScrollInfo ())
        redrawTileBuffer <- true

    static member val VerticalOffsetProperty =
        Dp.reg<int, ByteTile> "VerticalOffset"
            (Dp.Meta (0, Dp.MetaFlags.AffectsRender, fun o (oldValue, newValue) -> (o : ByteTile).OnVerticalOffsetChanged (oldValue, newValue)))

    member x.VerticalOffset
        with get () = x.GetValue ByteTile.VerticalOffsetProperty :?> int
        and set v = x.SetValue (ByteTile.VerticalOffsetProperty, box (v : int))

    member x.OnVerticalOffsetChanged (oldValue, newValue) =
        x.ScrollOwner |> Option.iter (fun owner -> owner.InvalidateScrollInfo ())
        redrawTileBuffer <- true

    member val CanHorizontallyScroll = true with get, set
    member val CanVerticallyScroll = true with get, set
    member val ScrollOwner : ScrollViewer option = None with get, set

    member x.TotalRowCount = int (ceil (float x.Bytes.Length / float x.CharsPerRow))
    member x.VisibleRowCount = vviewport

    member x.ExtentWidth = float ((byteCodeWidth + charWidth) * x.CharsPerRow + rowNumberWidth + columnSpacing * 3)
    member x.ViewportWidth = hviewport
    member x.HorizontalOffset = hoffset

    member x.SetHorizontalOffset offset =
        hoffset <- max 0.0 offset
        x.ScrollOwner |> Option.iter (fun owner -> owner.InvalidateScrollInfo ())
        redrawTileBuffer <- true
        x.InvalidateVisual ()

    member x.SetVerticalOffset offset =
        x.VerticalOffset <- max 0 offset

    interface IScrollInfo with
        member x.CanHorizontallyScroll
            with get () = x.CanHorizontallyScroll
            and set v = x.CanHorizontallyScroll <- v

        member x.CanVerticallyScroll
            with get () = x.CanVerticallyScroll
            and set v = x.CanVerticallyScroll <- v

        member x.ScrollOwner
            with get () =
                match x.ScrollOwner with
                | Some sc -> sc
                | None -> null

            and set v =
                x.ScrollOwner <-
                    match v with
                    | null -> None
                    | _ -> Some v

        member x.ExtentHeight = float (max 0 (x.TotalRowCount + x.VisibleRowCount - 1))
        member x.ExtentWidth  = x.ExtentWidth
        member x.ViewportHeight = float x.VisibleRowCount
        member x.ViewportWidth  = x.ViewportWidth
        member x.HorizontalOffset = x.HorizontalOffset
        member x.VerticalOffset   = float x.VerticalOffset

        member x.SetHorizontalOffset offset = x.SetHorizontalOffset offset
        member x.SetVerticalOffset offset = x.SetVerticalOffset (int offset)

        member x.LineDown        () = x.SetVerticalOffset   (x.VerticalOffset   + 1)
        member x.LineLeft        () = x.SetHorizontalOffset (x.HorizontalOffset - x.ViewportWidth  * 0.1)
        member x.LineRight       () = x.SetHorizontalOffset (x.HorizontalOffset + x.ViewportWidth  * 0.1)
        member x.LineUp          () = x.SetVerticalOffset   (x.VerticalOffset   - 1)
        member x.MouseWheelDown  () = x.SetVerticalOffset   (x.VerticalOffset   + 1)
        member x.MouseWheelLeft  () = x.SetHorizontalOffset (x.HorizontalOffset - x.ViewportWidth  * 0.1)
        member x.MouseWheelRight () = x.SetHorizontalOffset (x.HorizontalOffset + x.ViewportWidth  * 0.1)
        member x.MouseWheelUp    () = x.SetVerticalOffset   (x.VerticalOffset   - 1)
        member x.PageDown        () = x.SetVerticalOffset   (x.VerticalOffset   + max 1 (x.VisibleRowCount - 1))
        member x.PageLeft        () = x.SetHorizontalOffset (x.HorizontalOffset - x.ViewportWidth  * 0.9)
        member x.PageRight       () = x.SetHorizontalOffset (x.HorizontalOffset + x.ViewportWidth  * 0.9)
        member x.PageUp          () = x.SetVerticalOffset   (x.VerticalOffset   - max 1 (x.VisibleRowCount - 1))

        member x.MakeVisible (visual, rectangle) = Rect ()

    member x.PositionToByteTile (mousePos : Point) =
        let pos = mousePos + Vector (hoffset, 0.0)
        let row = int pos.Y / charHeight + x.VerticalOffset

        let columnRange = Range.ofLength (rowNumberWidth + columnSpacing) (byteCodeWidth * x.CharsPerRow)
        let col = (int pos.X - columnRange.Start) / byteCodeWidth

        let normalizedRow = row |> clamp 0 (x.TotalRowCount - 1)
        let normalizedCol = col |> clamp 0 (x.CharsPerRow - 1)
        let blockIndex = normalizedCol + normalizedRow * x.CharsPerRow
        let normalizedIndex = blockIndex |> clamp 0 (x.Bytes.Length - 1)

        let isInside = col = normalizedCol && row = normalizedRow && blockIndex = normalizedIndex
        normalizedIndex, isInside

    member x.SnapDoubleByteChar index =
        if charByteLayout.[index] = PrintDouble then
            OverDoubleByte index
        elif index > 0 && charByteLayout.[index - 1] = PrintDouble then
            OverDoubleByte (index - 1)
        else
            OverSingleByte index

    member x.PositionToCharTile (mousePos : Point) =
        let pos = mousePos + Vector (hoffset, 0.0)
        let row = int pos.Y / charHeight + x.VerticalOffset

        let columnRange = Range.ofLength (rowNumberWidth + byteCodeWidth * x.CharsPerRow + columnSpacing * 2) (charWidth * x.CharsPerRow)
        let col = (int pos.X - columnRange.Start) / charWidth

        let normalizedRow = row |> clamp 0 (x.TotalRowCount - 1)

        let rowHasOverflow =
            let index = (normalizedRow + 1) * x.CharsPerRow
            index > 0 && index < x.Bytes.Length && charByteLayout.[index - 1] = PrintDouble

        let normalizedCol = col |> clamp 0 (if rowHasOverflow then x.CharsPerRow else x.CharsPerRow - 1)
        let blockIndex = normalizedCol + normalizedRow * x.CharsPerRow
        let normalizedIndex = blockIndex |> clamp 0 (x.Bytes.Length - 1)

        let isInside = col = normalizedCol && row = normalizedRow && blockIndex = normalizedIndex
        let index = x.SnapDoubleByteChar normalizedIndex
        index, isInside

    override x.OnMouseDown e =
        base.OnMouseDown e

        let mouseDownButton =
            match e.ChangedButton with
            | MouseButton.Left -> Some (LeftButton ())
            | MouseButton.Right -> Some (RightButton ())
            | _ -> None

        mouseDownButton
        |> Option.iter (fun mouseDownButton ->
            let captured = x.CaptureMouse ()
            if captured then
                let mouseDownStatus =
                    let mousePos = e.GetPosition x
                    let mouseDownTile =
                        match x.PositionToByteTile mousePos with
                        | index, true -> Some (OverSingleByte index, ByteColumn)
                        | _, false ->
                            match x.PositionToCharTile mousePos with
                            | index, true -> Some (index, CharColumn)
                            | _, false -> None

                    match mouseDownButton with
                    | LeftButton () when e.ClickCount = 1 ->
                        mouseDownTile
                        |> Option.bind (fun (mouseDownByte, tileColumn) ->
                            let mouseDownRange = mouseDownByte.AsRange
                            x.ScriptFile
                            |> Seq.tryPick (fun trsc ->
                                let trscRange = trsc.Range
                                if mouseDownRange.Contains trscRange.Start then
                                    let rangeAnchor = x.SnapDoubleByteChar (trscRange.End - 1)
                                    Some (Some (trsc, rangeAnchor.AsRange, tileColumn))
                                elif mouseDownRange.Contains (trscRange.End - 1) then
                                    let rangeAnchor = x.SnapDoubleByteChar trscRange.Start
                                    Some (Some (trsc, rangeAnchor.AsRange, tileColumn))
                                elif Option.isSome (mouseDownRange.Intersect trscRange) then
                                    Some None
                                else
                                    None)
                            |> Option.getOrGetFallback (fun () ->
                                let index =
                                    x.ScriptFile
                                    |> Seq.tryFindIndex (fun trsc -> trsc.Range.Start >= mouseDownRange.End)
                                    |> Option.getOrFallbackTo x.ScriptFile.Count
                                let trsc = x.ScriptFile.InsertNew index mouseDownRange
                                Some (trsc, mouseDownRange, tileColumn)))
                        |> LeftButton

                    | LeftButton () ->
                        mouseDownTile
                        |> Option.iter (fun (mouseDownByte, _) ->
                            x.ScriptFile
                            |> Seq.tryFind (fun trsc -> Option.isSome (mouseDownByte.AsRange.Intersect trsc.Range))
                            |> Option.iter (fun trsc ->
                                let baseRange = trsc.Range.Union mouseDownByte.AsRange
                                let bytes = x.Bytes
                                let maxRange = Range.ofStartEnd (0, bytes.Length)
                                let rec expand checkDelta increment offset =
                                    let newOffset = offset + checkDelta
                                    if maxRange.Contains newOffset && maxRange.Contains (newOffset + 1) &&
                                        (charByteLayout.[newOffset] = PrintDouble ||
                                            (bytes.[newOffset] = byte '\\' && bytes.[newOffset + 1] = byte 'n')) then
                                        expand checkDelta increment (offset + increment)
                                    else offset
                                let newRange =
                                    let newStartValue = min (expand (-2) (-2) baseRange.Start) (expand (-2) (-2) (baseRange.Start + 1))
                                    let newEndValue = max (expand 0 2 baseRange.End) (expand 0 2 (baseRange.End - 1))
                                    Range.ofStartEnd (newStartValue, newEndValue)
                                    |> Range.intersect (x.ScriptFile.GetMaxRange trsc)
                                    |> Option.get
                                trsc.Range <- newRange))

                        LeftButton None

                    | RightButton () ->
                        mouseDownTile
                        |> Option.bind (fun (mouseDownByte, tileColumn) ->
                            let mouseDownRange = mouseDownByte.AsRange
                            x.ScriptFile
                            |> Seq.tryFind (fun trsc ->
                                Option.isSome (mouseDownRange.Intersect trsc.Range))
                            |> Option.map (fun trsc -> trsc, tileColumn, true))
                        |> RightButton

                mouseStatus <- MousePressed mouseDownStatus)

        redrawDispBuffer <- true
        x.InvalidateVisual ()

    override x.OnMouseMove e =
        base.OnMouseMove e

        let pos = e.GetPosition x
        mouseStatus <-
            match mouseStatus with
            | MouseReleased _ ->
                match x.PositionToByteTile pos with
                | index, true -> MouseReleased (Some (OverSingleByte index))
                | _, false ->
                    match x.PositionToCharTile pos with
                    | index, true -> MouseReleased (Some index)
                    | _, false -> MouseReleased None

            | MousePressed mouseDownStatus ->
                match mouseDownStatus with
                | LeftButton (Some (trsc, rangeAnchor, tileColumn)) ->
                    let mouseOverTile =
                        match tileColumn with
                        | ByteColumn ->
                            let index, _ = x.PositionToByteTile pos
                            OverSingleByte index
                        | CharColumn ->
                            let index, _ = x.PositionToCharTile pos
                            index
                    let newRange =
                        rangeAnchor.Union mouseOverTile.AsRange
                        |> Range.intersect (x.ScriptFile.GetMaxRange trsc)
                        |> Option.get
                    trsc.Range <- newRange
                    mouseDownStatus

                | RightButton (Some (trsc, tileColumn, isDirectlyOver)) ->
                    let mouseOverTile, isInsideColumn =
                        match tileColumn with
                        | ByteColumn ->
                            let index, isInsideColumn = x.PositionToByteTile pos
                            OverSingleByte index, isInsideColumn
                        | CharColumn ->
                            x.PositionToCharTile pos
                    let isDirectlyOver = isInsideColumn && Option.isSome (trsc.Range.Intersect mouseOverTile.AsRange)
                    RightButton (Some (trsc, tileColumn, isDirectlyOver))

                | LeftButton None
                | RightButton None ->
                    mouseDownStatus

                |> MousePressed

        redrawDispBuffer <- true
        x.InvalidateVisual ()

    override x.OnMouseUp e =
        x.ReleaseMouseCapture ()

        base.OnMouseUp e

    override x.OnGotMouseCapture e =
        base.OnGotMouseCapture e

    override x.OnLostMouseCapture e =
        base.OnLostMouseCapture e

        let mousePos = e.GetPosition x
        mouseStatus <-
            match mouseStatus with
            | MouseReleased _ ->
                mouseStatus

            | MousePressed mouseDownStatus ->
                match mouseDownStatus with
                | LeftButton (Some (trsc, rangeAnchor, tileColumn)) -> ()

                | RightButton (Some (trsc, tileColumn, isDirectlyOver)) ->
                    let remove =
                        match tileColumn with
                        | ByteColumn ->
                            let index, isInside = x.PositionToByteTile mousePos
                            isInside && trsc.Range.Contains index
                        | CharColumn ->
                            let index, isInside = x.PositionToCharTile mousePos
                            isInside && Option.isSome (trsc.Range.Intersect index.AsRange)
                    if remove then
                        x.ScriptFile.Remove trsc

                | _ -> ()

                MouseReleased None

        redrawDispBuffer <- true
        x.InvalidateVisual ()

    override x.OnMouseLeave e =
        base.OnMouseLeave e

        mouseStatus <-
            match mouseStatus with
            | MouseReleased _ ->
                MouseReleased None

            | MousePressed _ ->
                mouseStatus

        redrawDispBuffer <- true
        x.InvalidateVisual ()

    override x.MeasureOverride size =
        base.MeasureOverride size

    override x.ArrangeOverride size =
        match buffer with
        | None when int size.Width = 0 || int size.Height = 0 -> ()
        | Some buffer when buffer.Width = int size.Width && buffer.Height = int size.Height -> ()
        | _ ->
            buffer <-
                if x.Bytes.Length = 0 then None
                elif int size.Width = 0 || int size.Height = 0 then None
                else Some (BitmapBuffer (int size.Width, int size.Height))
            GC.Collect ()

            hviewport <- size.Width
            vviewport <- int (ceil (size.Height / float charHeight))
            x.ScrollOwner |> Option.iter (fun owner -> owner.InvalidateScrollInfo ())
            redrawTileBuffer <- true

        size

    override x.OnRender dc =
        base.OnRender dc

        buffer
        |> Option.iter (fun buffer ->
            let bytes = x.Bytes
            let charsPerRow = x.CharsPerRow
            let voffset = min x.VerticalOffset (int (Math.Ceiling (float bytes.Length / float charsPerRow)) - 1)
            let rowCount = x.VisibleRowCount
            let startCharIndex = voffset * charsPerRow
            let visibleChars = Range.ofStartEnd (startCharIndex, min (startCharIndex + rowCount * charsPerRow) bytes.Length)

            if redrawTileBuffer then
                buffer.DrawTile <| fun surface ->
                    let canvas = surface.Canvas
                    canvas.Clear (SKColor (0xFFuy, 0xFFuy, 0xFFuy))
                    let restoreMat = canvas.TotalMatrix

                    canvas.Translate (float32 (-hoffset), 0.0f)

                    for i in voffset .. min (voffset + rowCount) x.TotalRowCount - 1 do
                        canvas.DrawText (
                            sprintf "%08X" (i * charsPerRow),
                            0.0f,
                            float32 ((i - voffset) * charHeight + rowNumberVOffset),
                            rowNumberPaint)

                    canvas.Translate (float32 (rowNumberWidth + columnSpacing), 0.0f)

                    for i in visibleChars.Start .. visibleChars.End - 1 do
                        let sourceRect =
                            let offset = getByteDispOffset charsPerRow (int bytes.[i])
                            SKRect.Create (float32 offset.X, float32 offset.Y, float32 byteCodeWidth, float32 charHeight)
                        let destRect =
                            let offset = getByteDispOffset charsPerRow i
                            SKRect.Create (float32 offset.X, float32 (offset.Y - voffset * charHeight), float32 byteCodeWidth, float32 charHeight)
                        canvas.DrawBitmap (byteTileBitmap, sourceRect, destRect)

                    canvas.Translate (float32 (charsPerRow * byteCodeWidth + columnSpacing), 0.0f)

                    for i in visibleChars.Start .. visibleChars.End - 1 do
                        match charByteLayout.[i] with
                        | Unprinted -> ()
                        | PrintSingle ->
                            let charSourceOffset = getCharDispOffset charsPerRow (int bytes.[i])
                            let charDestOffset = getCharDispOffset charsPerRow i
                            let sourceRect = SKRect.Create (float32 charSourceOffset.X, float32 charSourceOffset.Y, float32 charWidth, float32 charHeight)
                            let destRect = SKRect.Create (float32 charDestOffset.X, float32 (charDestOffset.Y - voffset * charHeight), float32 charWidth, float32 charHeight)
                            canvas.DrawBitmap (charTileBitmap, sourceRect, destRect)
                        | PrintDouble ->
                            let charDestOffset = getCharDrawOffset charsPerRow kanjiVOffset true i
                            canvas.DrawText (
                                shiftJis.GetString [| bytes.[i]; bytes.[i + 1] |],
                                charDestOffset.X,
                                charDestOffset.Y - float32 (voffset * charHeight),
                                kanjiPaint)

                    canvas.SetMatrix restoreMat

                redrawTileBuffer <- false
                redrawDispBuffer <- true

            if redrawDispBuffer then
                buffer.DrawDisp <| fun surface ->
                    let canvas = surface.Canvas
                    canvas.DrawBitmap (buffer.TileBitmap, 0.0f, 0.0f)
                    use __ = canvas.SaveLoadCurrentTransform ()

                    let buildSelectionPath (range : Range) =
                        let isSameRow = range.Start / charsPerRow = (range.End - 1) / charsPerRow
                        let path = new SKPath ()
                        let buildPath startTop startLeft endBottom endRight leftMost rightMost =
                            if isSameRow then
                                path.MoveTo (float32 startLeft, float32 startTop)
                                path.LineTo (float32 endRight, float32 startTop)
                                path.LineTo (float32 endRight, float32 endBottom)
                                path.LineTo (float32 startLeft, float32 endBottom)
                            else
                                path.MoveTo (float32 startLeft, float32 (startTop + charHeight))
                                path.LineTo (float32 startLeft, float32 startTop)
                                path.LineTo (float32 rightMost, float32 startTop)
                                path.LineTo (float32 rightMost, float32 (endBottom - charHeight))
                                path.LineTo (float32 endRight, float32 (endBottom - charHeight))
                                path.LineTo (float32 endRight, float32 endBottom)
                                path.LineTo (float32 leftMost, float32 endBottom)
                                path.LineTo (float32 leftMost, float32 (startTop + charHeight))
                            path.Close ()
                        do  let leftMost = rowNumberWidth + columnSpacing
                            let rightMost = leftMost + charsPerRow * byteCodeWidth
                            let startTop = (range.Start / charsPerRow - voffset) * charHeight
                            let startLeft = (range.Start % charsPerRow) * byteCodeWidth + leftMost
                            let endBottom = ((range.End - 1) / charsPerRow - voffset + 1) * charHeight
                            let endRight = ((range.End - 1) % charsPerRow + 1) * byteCodeWidth + leftMost
                            buildPath startTop startLeft endBottom endRight leftMost rightMost
                        do  let leftMost = rowNumberWidth + charsPerRow * byteCodeWidth + columnSpacing * 2
                            let rightMost = leftMost + charsPerRow * charWidth
                            let startTop = (range.Start / charsPerRow - voffset) * charHeight
                            let startLeft = (range.Start % charsPerRow) * charWidth + leftMost
                            let endBottom = ((range.End - 1) / charsPerRow - voffset + 1) * charHeight
                            let endRight = ((range.End - 1) % charsPerRow + 1) * charWidth + leftMost
                            buildPath startTop startLeft endBottom endRight leftMost rightMost
                        path

                    canvas.Translate (float32 (-hoffset), 0.0f)

                    do  use __ = canvas.SaveLoadCurrentTransform ()
                        canvas.Translate (0.5f, 0.5f)
                        //let path = buildSelectionPath (Range.ofLength 120 22)
                        //canvas.DrawPath (path, selectionPaint)
                        //canvas.DrawPath (path, mouseOverPaint)

                        let paths =
                            x.ScriptFile
                            |> Seq.choose (fun trsc -> visibleChars.Intersect trsc.Range)
                            |> Seq.map buildSelectionPath
                            |> Seq.cache

                        for path in paths do canvas.DrawPath (path, selectionTintPaint)
                        for path in paths do canvas.DrawPath (path, selectionFillPaint)
                        for path in paths do canvas.DrawPath (path, selectionBorderPaint)

                        match mouseStatus with
                        | MouseReleased mouseOverStatus ->
                            mouseOverStatus
                            |> Option.iter (fun mouseOverStatus ->
                                let mouseOverRange = mouseOverStatus.AsRange
                                canvas.DrawPath (buildSelectionPath mouseOverRange, mouseOverPaint))

                        | MousePressed mouseDownStatus ->
                            match mouseDownStatus with
                            | LeftButton (Some (trsc, rangeAnchor, tileColumn)) ->
                                canvas.DrawPath (buildSelectionPath trsc.Range, mouseDownPaint)

                            | RightButton (Some (trsc, tileColumn, isDirectlyOver)) ->
                                if isDirectlyOver then
                                    canvas.DrawPath (buildSelectionPath trsc.Range, deletionBorderPaint)

                            | _ -> ()


                redrawDispBuffer <- false

            dc.DrawImage (buffer.DispBitmap, Rect (0.0, 0.0, float buffer.Width, float buffer.Height)))


