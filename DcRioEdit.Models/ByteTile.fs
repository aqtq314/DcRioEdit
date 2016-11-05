namespace DcRioEdit.Models

open SkiaSharp
open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Threading.Tasks
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Documents
open System.Windows.Input
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Navigation
open System.Windows.Shapes


//type BitmapBuffer (width, height) =
//    let bitmap = WriteableBitmap (width, height, 96.0, 96.0, PixelFormats.Pbgra32, null)
//    let surface = SKSurface.Create (540, 960, SKColorType.Bgra8888, SKAlphaType.Premul, bitmap.BackBuffer, bitmap.BackBufferStride)
//    let canvas = surface.Canvas
//
//    member x.Bitmap = bitmap
//    member x.SKSurface = surface
//    member x.SKCanvas = canvas
//
//    member x.Dispose () =
//        surface.Dispose ()


type ByteTile () =
    inherit FrameworkElement ()

    //static let byteCodeSheet =
    //    let bitmap = WriteableBitmap.Create (

    let mutable bitmap = null

    static member val BytesProperty =
        Dp.reg<ArraySeg<byte>, ByteTile> "Bytes"
            (Dp.Meta (ArraySeg.empty, fun o (oldValue, newValue) -> (o : ByteTile).OnBytesChanged (oldValue, newValue)))

    member x.Bytes
        with get () = x.GetValue ByteTile.BytesProperty :?> ArraySeg<byte>
        and set v = x.SetValue (ByteTile.BytesProperty, box (v : ArraySeg<byte>))

    member x.OnBytesChanged (oldValue, newValue) =
        ()

    override x.ArrangeOverride size =
        bitmap <-
            if int size.Width = 0 || int size.Height = 0 then null
            else WriteableBitmap (int size.Width, int size.Height, 96.0, 96.0, PixelFormats.Pbgra32, null)
        base.ArrangeOverride size

    override x.OnRender dc =
        base.OnRender dc

        if isNotNull bitmap then
            bitmap.Lock ()
            do  use surface = SKSurface.Create (bitmap.PixelWidth, bitmap.PixelHeight, SKColorType.Bgra8888, SKAlphaType.Premul, bitmap.BackBuffer, bitmap.BackBufferStride)
                let canvas = surface.Canvas
                canvas.Clear (SKColor (100uy, 149uy, 237uy))

            bitmap.AddDirtyRect (Int32Rect (0, 0, bitmap.PixelWidth, bitmap.PixelHeight))
            bitmap.Unlock ()

            dc.DrawImage (bitmap, Rect (0.0, 0.0, float bitmap.PixelWidth, float bitmap.PixelHeight))
        

