namespace DcRioEdit.Models

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
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Navigation
open System.Windows.Shapes

#nowarn "62"


[<AutoOpen>]
module WpfUtil =
    let inline freeze (g : #Freezable) = g.Freeze ()

    type VectorF with
        member x.AsWpfPoint = System.Windows.Point (x.X, x.Y)

        member x.AsWpfVector = System.Windows.Vector (x.X, x.Y)

        static member ofWpfPoint (p : System.Windows.Point) =
            VectorF (p.X, p.Y)

        static member ofWpfVector (p : System.Windows.Vector) =
            VectorF (p.X, p.Y)


module BooleanBoxes =
    let falseBox = (UIElement.FocusableProperty.GetMetadata typeof<UIElement>).DefaultValue
    let trueBox = (UIElement.FocusableProperty.GetMetadata typeof<Control>).DefaultValue


module internal Dp =
    type MetaFlags = FrameworkPropertyMetadataOptions

    type ('a, 'owner) Meta when 'owner :> DependencyObject private (meta : FrameworkPropertyMetadata) =
        new (initval : 'a) =
            Meta (FrameworkPropertyMetadata (initval))

        new (initval : 'a, flags : MetaFlags) =
            Meta (FrameworkPropertyMetadata (initval, flags))

        new (initval : 'a, flags : MetaFlags, propChanged) =
            Meta (
                FrameworkPropertyMetadata (
                    initval, flags,
                    PropertyChangedCallback (fun d e -> propChanged (d :?> 'owner) (e.OldValue :?> 'a, e.NewValue :?> 'a))))

        new (initval : 'a, flags : MetaFlags, propChanged, coerce : _ -> _ -> 'a) =
            Meta (
                FrameworkPropertyMetadata (
                    initval, flags,
                    PropertyChangedCallback (fun d e -> propChanged (d :?> 'owner) (e.OldValue :?> 'a, e.NewValue :?> 'a)),
                    CoerceValueCallback (fun d v -> coerce (d :?> 'owner) (v :?> 'a) |> box)))

        new (initval : 'a, flags : MetaFlags, propChanged, coerce : _ -> _ -> 'a, forbidAnimation) =
            Meta (
                FrameworkPropertyMetadata (
                    initval, flags,
                    PropertyChangedCallback (fun d e -> propChanged (d :?> 'owner) (e.OldValue :?> 'a, e.NewValue :?> 'a)),
                    CoerceValueCallback (fun d v -> coerce (d :?> 'owner) (v :?> 'a) |> box),
                    forbidAnimation))

        new (initval : 'a, flags : MetaFlags, propChanged, coerce : _ -> _ -> 'a, forbidAnimation, initTrigger) =
            Meta (
                FrameworkPropertyMetadata (
                    initval, flags,
                    PropertyChangedCallback (fun d e -> propChanged (d :?> 'owner) (e.OldValue :?> 'a, e.NewValue :?> 'a)),
                    CoerceValueCallback (fun d v -> coerce (d :?> 'owner) (v :?> 'a) |> box),
                    forbidAnimation, initTrigger))

        new (initval : 'a, propChanged) =
            Meta (
                FrameworkPropertyMetadata (
                    initval,
                    PropertyChangedCallback (fun d e -> propChanged (d :?> 'owner) (e.OldValue :?> 'a, e.NewValue :?> 'a))))

        new (initval : 'a, propChanged, coerce : _ -> _ -> 'a) =
            Meta (
                FrameworkPropertyMetadata (
                    initval,
                    PropertyChangedCallback (fun d e -> propChanged (d :?> 'owner) (e.OldValue :?> 'a, e.NewValue :?> 'a)),
                    CoerceValueCallback (fun d v -> coerce (d :?> 'owner) (v :?> 'a) |> box)))

        member x.WpfMeta = meta

    let inline reg<'a, 'owner when 'owner :> DependencyObject> name (meta : ('a, 'owner) Meta) =
        DependencyProperty.Register (
            name, typeof<'a>, typeof<'owner>, meta.WpfMeta)

    let inline regr<'a, 'owner when 'owner :> DependencyObject> name (meta : ('a, 'owner) Meta) =
        DependencyProperty.RegisterReadOnly (
            name, typeof<'a>, typeof<'owner>, meta.WpfMeta)

    let inline rega<'a, 'owner when 'owner :> DependencyObject> name (meta : ('a, DependencyObject) Meta) =
        DependencyProperty.RegisterAttached (
            name, typeof<'a>, typeof<'owner>, meta.WpfMeta)

    let inline regar<'a, 'owner when 'owner :> DependencyObject> name (meta : ('a, DependencyObject) Meta) =
        DependencyProperty.RegisterAttachedReadOnly (
            name, typeof<'a>, typeof<'owner>, meta.WpfMeta)


