﻿namespace DcRioEdit.Models

open System
open System.Collections.Generic
open System.ComponentModel
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


type Behaviors private () =
    inherit DependencyObject ()

    static member private DetachAttach d (oldValue, newValue) =
        if not (DesignerProperties.GetIsInDesignMode d) then
            let d = (d : DependencyObject) :?> _
            if isNotNull (box oldValue) then (oldValue : IBehavior<_>).OnDetach d
            if isNotNull (box newValue) then (newValue : IBehavior<_>).OnAttach d


    static member GetMouseBehavior (d : DependencyObject) = d.GetValue Behaviors.MouseBehaviorProperty :?> IBehavior<UIElement>
    static member SetMouseBehavior (d : DependencyObject, value : IBehavior<UIElement>) = d.SetValue (Behaviors.MouseBehaviorProperty, value)
    static member OnMouseBehaviorChanged d (oldValue, newValue) = Behaviors.DetachAttach d (oldValue, newValue)
    static member val MouseBehaviorProperty =
        Dp.rega<IBehavior<UIElement>, Behaviors> "MouseBehavior"
            (Dp.Meta (Behavior.none, Behaviors.OnMouseBehaviorChanged))


    static member GetKeyboardBehavior (d : DependencyObject) = d.GetValue Behaviors.KeyboardBehaviorProperty :?> IBehavior<UIElement>
    static member SetKeyboardBehavior (d : DependencyObject, value : IBehavior<UIElement>) = d.SetValue (Behaviors.KeyboardBehaviorProperty, value)
    static member OnKeyboardBehaviorChanged d (oldValue, newValue) = Behaviors.DetachAttach d (oldValue, newValue)
    static member val KeyboardBehaviorProperty =
        Dp.rega<IBehavior<UIElement>, Behaviors> "KeyboardBehavior"
            (Dp.Meta (Behavior.none, Behaviors.OnKeyboardBehaviorChanged))


    static member GetPreviewKeyboardBehavior (d : DependencyObject) = d.GetValue Behaviors.PreviewKeyboardBehaviorProperty :?> IBehavior<UIElement>
    static member SetPreviewKeyboardBehavior (d : DependencyObject, value : IBehavior<UIElement>) = d.SetValue (Behaviors.PreviewKeyboardBehaviorProperty, value)
    static member OnPreviewKeyboardBehaviorChanged d (oldValue, newValue) = Behaviors.DetachAttach d (oldValue, newValue)
    static member val PreviewKeyboardBehaviorProperty =
        Dp.rega<IBehavior<UIElement>, Behaviors> "PreviewKeyboardBehavior"
            (Dp.Meta (Behavior.none, Behaviors.OnPreviewKeyboardBehaviorChanged))


    static member GetDragBehavior (d : DependencyObject) = d.GetValue Behaviors.DragBehaviorProperty :?> IBehavior<Thumb>
    static member SetDragBehavior (d : DependencyObject, value : IBehavior<Thumb>) = d.SetValue (Behaviors.DragBehaviorProperty, value)
    static member OnDragBehaviorChanged d (oldValue, newValue) = Behaviors.DetachAttach d (oldValue, newValue)
    static member val DragBehaviorProperty =
        Dp.rega<IBehavior<Thumb>, Behaviors> "DragBehavior"
            (Dp.Meta (Behavior.none, Behaviors.OnDragBehaviorChanged))


    static member GetMouseWheelBehavior (d : DependencyObject) = d.GetValue Behaviors.MouseWheelBehaviorProperty :?> IBehavior<UIElement>
    static member SetMouseWheelBehavior (d : DependencyObject, value : IBehavior<UIElement>) = d.SetValue (Behaviors.MouseWheelBehaviorProperty, value)
    static member OnMouseWheelBehaviorChanged d (oldValue, newValue) = Behaviors.DetachAttach d (oldValue, newValue)
    static member val MouseWheelBehaviorProperty =
        Dp.rega<IBehavior<UIElement>, Behaviors> "MouseWheelBehavior"
            (Dp.Meta (Behavior.none, Behaviors.OnMouseWheelBehaviorChanged))


    static member GetClickBehavior (d : DependencyObject) = d.GetValue Behaviors.ClickBehaviorProperty :?> IBehavior<ButtonBase>
    static member SetClickBehavior (d : DependencyObject, value : IBehavior<ButtonBase>) = d.SetValue (Behaviors.ClickBehaviorProperty, value)
    static member OnClickBehaviorChanged d (oldValue, newValue) = Behaviors.DetachAttach d (oldValue, newValue)
    static member val ClickBehaviorProperty =
        Dp.rega<IBehavior<ButtonBase>, Behaviors> "ClickBehavior"
            (Dp.Meta (Behavior.none, Behaviors.OnClickBehaviorChanged))


    static member GetMenuItemClickBehavior (d : DependencyObject) = d.GetValue Behaviors.MenuItemClickBehaviorProperty :?> IBehavior<MenuItem>
    static member SetMenuItemClickBehavior (d : DependencyObject, value : IBehavior<MenuItem>) = d.SetValue (Behaviors.MenuItemClickBehaviorProperty, value)
    static member OnMenuItemClickBehaviorChanged d (oldValue, newValue) = Behaviors.DetachAttach d (oldValue, newValue)
    static member val MenuItemClickBehaviorProperty =
        Dp.rega<IBehavior<MenuItem>, Behaviors> "MenuItemClickBehavior"
            (Dp.Meta (Behavior.none, Behaviors.OnMenuItemClickBehaviorChanged))


    static member GetMouseHoverBehavior (d : DependencyObject) = d.GetValue Behaviors.MouseHoverBehaviorProperty :?> IBehavior<UIElement>
    static member SetMouseHoverBehavior (d : DependencyObject, value : IBehavior<UIElement>) = d.SetValue (Behaviors.MouseHoverBehaviorProperty, value)
    static member OnMouseHoverBehaviorChanged d (oldValue, newValue) = Behaviors.DetachAttach d (oldValue, newValue)
    static member val MouseHoverBehaviorProperty =
        Dp.rega<IBehavior<UIElement>, Behaviors> "MouseHoverBehavior"
            (Dp.Meta (Behavior.none, Behaviors.OnMouseHoverBehaviorChanged))


    static member GetRangeBaseValueChangedBehavior (d : DependencyObject) = d.GetValue Behaviors.RangeBaseValueChangedBehaviorProperty :?> IBehavior<RangeBase>
    static member SetRangeBaseValueChangedBehavior (d : DependencyObject, value : IBehavior<RangeBase>) = d.SetValue (Behaviors.RangeBaseValueChangedBehaviorProperty, value)
    static member OnRangeBaseValueChangedBehaviorChanged d (oldValue, newValue) = Behaviors.DetachAttach d (oldValue, newValue)
    static member val RangeBaseValueChangedBehaviorProperty =
        Dp.rega<IBehavior<RangeBase>, Behaviors> "RangeBaseValueChangedBehavior"
            (Dp.Meta (Behavior.none, Behaviors.OnRangeBaseValueChangedBehaviorChanged))


