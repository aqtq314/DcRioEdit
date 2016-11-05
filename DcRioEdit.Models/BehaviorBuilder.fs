namespace DcRioEdit.Models

open System
open System.Collections.Generic
open System.Threading.Tasks
open System.Windows
open System.Windows.Controls
open System.Windows.Controls.Primitives
open System.Windows.Input

#nowarn "40"
#nowarn "21"


[<ReferenceEquality>]
[<NoComparison>]
type 'e BehaviorAction =
    | BehaviorAction of action : ('e -> 'e BehaviorAction)

    member x.Run e =
        let (BehaviorAction action) = x
        action e

type BehaviorBuilder () =
    member x.Bind ((), f) =
        BehaviorAction f

    member x.Zero () = ()

    member x.Combine ((), f : _ BehaviorAction) =
        f

    member x.Delay f : _ BehaviorAction =
        f ()

    member x.ReturnFrom f : _ BehaviorAction =
        f

    member x.Using (m, f) =
        use m = m
        f m

[<AutoOpen>]
module BehaviorBuilderUtil =
    let behavior = BehaviorBuilder ()


type IBehavior<'d> =
    abstract OnAttach : target : 'd -> unit
    abstract OnDetach : target : 'd -> unit


module Behavior =
    let none<'d> =
        { new IBehavior<'d> with
            member x.OnAttach _ = ()
            member x.OnDetach _ = () }

    let inline private createBehaviorHandler handler (event : IEvent<_, _>) =
        (fun () -> event.AddHandler handler),
        (fun () -> event.RemoveHandler handler)

    let createBehavior getHandlers (action : _ BehaviorAction) =
        let action = ref action
        let push e =
            let newAction = (!action).Run e
            action := newAction

        let behaviorMap = Dictionary<_, _> ()

        { new IBehavior<_> with
            member x.OnAttach u =
                let attach, detach =
                    let handlers = getHandlers u push |> Array.ofSeq
                    (fun () -> Array.iter (fun (attach, detach) -> attach ()) handlers),
                    (fun () -> Array.iter (fun (attach, detach) -> detach ()) handlers)
                behaviorMap.Add (u, detach)
                attach ()

            member x.OnDetach u =
                let detach = behaviorMap.[u]
                detach ()
                behaviorMap.Remove u |> ignore }

    let mouseBehavior action =
        action
        |> createBehavior (fun (u : UIElement) push -> seq {
            yield u.MouseDown |> createBehaviorHandler (MouseButtonEventHandler (fun sender e ->
                push (BehaviorMouseEventArgs (VectorF.ofWpfPoint (e.GetPosition u), MouseDownEvent (e.ChangedButton, e.ClickCount), (fun () -> e.Handled <- true)))))

            yield u.MouseMove |> createBehaviorHandler (MouseEventHandler (fun sender e ->
                push (BehaviorMouseEventArgs (VectorF.ofWpfPoint (e.GetPosition u), MouseMoveEvent, (fun () -> e.Handled <- true)))))

            yield u.MouseUp |> createBehaviorHandler (MouseButtonEventHandler (fun sender e ->
                push (BehaviorMouseEventArgs (VectorF.ofWpfPoint (e.GetPosition u), MouseUpEvent (e.ChangedButton), (fun () -> e.Handled <- true))))) })
        
    let keyboardBehavior action =
        action
        |> createBehavior (fun (u : UIElement) push -> seq {
            yield u.KeyDown |> createBehaviorHandler (KeyEventHandler (fun _ e ->
                push (BehaviorKeyboardEventArgs (e, KeyDownEvent))))

            yield u.KeyUp |> createBehaviorHandler (KeyEventHandler (fun _ e ->
                push (BehaviorKeyboardEventArgs (e, KeyUpEvent)))) })
        
    let previewKeyboardBehavior action =
        action
        |> createBehavior (fun (u : UIElement) push -> seq {
            yield u.PreviewKeyDown |> createBehaviorHandler (KeyEventHandler (fun _ e ->
                push (BehaviorPreviewKeyboardEventArgs (e, PreviewKeyDownEvent))))

            yield u.PreviewKeyUp |> createBehaviorHandler (KeyEventHandler (fun _ e ->
                push (BehaviorPreviewKeyboardEventArgs (e, PreviewKeyUpEvent)))) })

    let dragBehavior action =
        action
        |> createBehavior (fun (u : Thumb) push -> seq {
            yield u.DragStarted |> createBehaviorHandler (DragStartedEventHandler (fun _ e ->
                push (BehaviorDragEventArgs (Keyboard.Modifiers, DragStartEvent (vec e.HorizontalOffset e.VerticalOffset)))))

            yield u.DragDelta |> createBehaviorHandler (DragDeltaEventHandler (fun _ e ->
                push (BehaviorDragEventArgs (Keyboard.Modifiers, DragDeltaEvent (vec e.HorizontalChange e.VerticalChange)))))

            yield u.DragCompleted |> createBehaviorHandler (DragCompletedEventHandler (fun _ e ->
                push (BehaviorDragEventArgs (Keyboard.Modifiers, DragCompletedEvent)))) })

    let mouseWheelBehavior action =
        action
        |> createBehavior (fun (u : UIElement) push -> seq {
            yield u.MouseWheel |> createBehaviorHandler (MouseWheelEventHandler (fun _ e ->
                push (BehaviorMouseWheelEventArgs (e.Delta, e.GetPosition u, (fun () -> e.Handled <- true))))) })

    let clickBehavior action =
        action
        |> createBehavior (fun (u : ButtonBase) push -> seq {
            yield u.Click |> createBehaviorHandler (RoutedEventHandler (fun _ e ->
                push (BehaviorClickEventArgs ()))) })

    let menuItemClickBehavior action =
        action
        |> createBehavior (fun (u : MenuItem) push -> seq {
            yield u.Click |> createBehaviorHandler (RoutedEventHandler (fun _ e ->
                push (BehaviorClickEventArgs ()))) })

    let mouseHoverBehavior action =
        action
        |> createBehavior (fun (u : UIElement) push -> seq {
            yield u.MouseEnter |> createBehaviorHandler (MouseEventHandler (fun _ e ->
                push (BehaviorMouseHoverEventArgs (true, MouseEnterEvent))))
            yield u.MouseLeave |> createBehaviorHandler (MouseEventHandler (fun _ e ->
                push (BehaviorMouseHoverEventArgs (false, MouseLeaveEvent)))) })

    let rangeBaseValueChangedBehavior action =
        action
        |> createBehavior (fun (u : RangeBase) push -> seq {
            yield u.ValueChanged |> createBehaviorHandler (RoutedPropertyChangedEventHandler (fun _ e ->
                push (BehaviorValueChangedEventArgs (e.NewValue)))) })



