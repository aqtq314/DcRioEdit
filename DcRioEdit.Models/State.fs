namespace DcRioEdit.Models

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Collections.Specialized
open System.ComponentModel
open System.Diagnostics


type IStateSubscriber =
    abstract Lock : unit -> unit
    abstract Unlock : isUpdateRequested : bool -> unit

type IParentState =
    abstract Subscribe : child : IStateSubscriber -> unit
    abstract Unsubscribe : child : IStateSubscriber -> unit

[<AutoOpen>]
module StateSubscriberUtil =
    type IStateSubscriber with
        member x.ForceUpdate () =
            x.Lock ()
            x.Unlock true

module StateSubscriber =
    let create update =
        let mutable lockCount = 0u
        let mutable isUpdateRequested = false

        { new IStateSubscriber with
            member __.Lock () =
                lockCount <- Checked.(+) lockCount 1u

            member __.Unlock newIsUpdateRequested =
                isUpdateRequested <- isUpdateRequested || newIsUpdateRequested
                lockCount <- Checked.(-) lockCount 1u

                if lockCount = 0u && isUpdateRequested then
                    update ()
                    isUpdateRequested <- false }


type IReorderableCollection =
    abstract Move : oldIndex : int -> newIndex : int -> unit
    abstract MoveRange : oldIndex : int -> newIndex : int -> count : int -> unit


type [<AbstractClass>] 'a stateBase () =
    abstract Value : 'a

    [<CLIEvent>]
    abstract PropertyChanged : IEvent<PropertyChangedEventHandler, PropertyChangedEventArgs>

    abstract Subscribe : child : IStateSubscriber -> unit
    abstract Unsubscribe : child : IStateSubscriber -> unit

    override x.ToString () = sprintf "st %A" x.Value

    interface IParentState with
        member x.Subscribe child = x.Subscribe child
        member x.Unsubscribe child = x.Unsubscribe child

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = x.PropertyChanged

type [<AbstractClass>] 'a writableStateBase () =
    inherit ('a stateBase) ()

    abstract SetValue : newValue : 'a -> unit


[<NoComparison>]
[<StructuralEquality>]
type CollectionStateChange<'a> =
    | AddItems of items : ArraySeg<'a> * index : int
    | MoveItems of items : ArraySeg<'a> * oldIndex : int * newIndex : int
    | RemoveItems of items : ArraySeg<'a> * index : int
    | ReplaceItems of oldItems : ArraySeg<'a> * newItems : ArraySeg<'a> * index : int

    member x.ToNotifyCollectionChangedEventArgs () =
        match x with
        | AddItems (items, index) ->
            NotifyCollectionChangedEventArgs (NotifyCollectionChangedAction.Add, ArraySeg.toBoxedIList items, index)
        | MoveItems (items, oldIndex, newIndex) ->
            NotifyCollectionChangedEventArgs (NotifyCollectionChangedAction.Move, ArraySeg.toBoxedIList items, newIndex, oldIndex)
        | RemoveItems (items, index) ->
            NotifyCollectionChangedEventArgs (NotifyCollectionChangedAction.Remove, ArraySeg.toBoxedIList items, index)
        | ReplaceItems (oldItems, newItems, index) ->
            NotifyCollectionChangedEventArgs (NotifyCollectionChangedAction.Replace, ArraySeg.toBoxedIList newItems, ArraySeg.toBoxedIList oldItems, index)

    member x.CountChanged =
        match x with
        | AddItems (items, index) -> true
        | MoveItems (items, oldIndex, newIndex) -> false
        | RemoveItems (items, index) -> true
        | ReplaceItems (oldItems, newItems, index) -> false

    member x.ItemsChanged = true

type ICollectionSubscriber<'a> =
    abstract Update : arg : CollectionStateChange<'a> -> unit

module CollectionSubscriber =
    let inline create update =
        { new ICollectionSubscriber<_> with
            member x.Update arg = update arg }


type 'a state when 'a : equality internal (value : 'a, sender) =
    inherit ('a writableStateBase) ()

    let propChangedEvent = Event<PropertyChangedEventHandler, _> ()
    let subscribers = List<IStateSubscriber> ()

    let mutable value = value

    let mutable lockCount = 0u
    let mutable requestSetValue = None

    [<CLIEvent>]
    override x.PropertyChanged = propChangedEvent.Publish

    override x.Value = value

    override x.Subscribe child =
        subscribers.Add child

    override x.Unsubscribe child =
        let index = subscribers.IndexOf child
        if index < 0 then
            raise (KeyNotFoundException ())
        subscribers.RemoveAt index

    member x.Lock () =
        if lockCount = 0u then
            subscribers
            |> Seq.iter (fun child -> child.Lock ())

        lockCount <- Checked.(+) lockCount 1u

        Disposable.createOnce x.Unlock

    member internal x.Unlock () =
        lockCount <- Checked.(-) lockCount 1u

        if lockCount = 0u then
            let valueChanged =
                match requestSetValue with
                | Some getValue ->
                    let newValue = getValue ()
                    let valueChanged = newValue <> value
                    if valueChanged then
                        value <- newValue
                    valueChanged

                | None ->
                    false

            subscribers
            |> Seq.iter (fun s -> s.Unlock valueChanged)

            if valueChanged then
                let sender = sender |> Option.getOrFallbackTo (box x)
                propChangedEvent.Trigger (sender, PropertyChangedEventArgs "Value")

            requestSetValue <- None

    member internal x.RequestSetValue getValue =
        use __ = x.Lock ()
        requestSetValue <- Some getValue

    override x.SetValue newValue =
        x.RequestSetValue (fun () -> newValue)

    member x.LockCount = lockCount


type 'a stateWpfWritable when 'a : equality internal (value : 'a, onValueChanged : 'a -> unit) =
    let propChangedEvent = Event<PropertyChangedEventHandler, _> ()

    let value = ref value

    [<CLIEvent>]
    member x.PropertyChanged = propChangedEvent.Publish

    member private x.SetValue silentMode newValue =
        let oldValue = !value
        if oldValue <> newValue then
            value := newValue
            if not silentMode then
                onValueChanged newValue
            propChangedEvent.Trigger (x, PropertyChangedEventArgs "Value")

    member x.Value
        with get () = !value
        and set newValue = x.SetValue false newValue

    member x.SetValueSilent newValue =
        x.SetValue true newValue

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = x.PropertyChanged


type [<AbstractClass>] 'a collectionStateBase () =
    [<CLIEvent>] abstract CollectionChanged : IEvent<NotifyCollectionChangedEventHandler, NotifyCollectionChangedEventArgs>
    [<CLIEvent>] abstract PropertyChanged : IEvent<PropertyChangedEventHandler, PropertyChangedEventArgs>
    abstract Item : index : int -> 'a with get
    abstract Count : int
    abstract Subscribe : state : ICollectionSubscriber<'a> -> unit
    abstract Unsubscribe : state : ICollectionSubscriber<'a> -> unit
    abstract GetEnumerator : unit -> IEnumerator<'a>
    abstract Contains : item : 'a -> bool
    abstract IndexOf : item : 'a -> int

    interface IReadOnlyList<'a> with
        member x.Item with get index = x.[index]

    interface IReadOnlyCollection<'a> with
        member x.Count = x.Count

    interface IEnumerable<'a> with
        member x.GetEnumerator () = x.GetEnumerator ()

    interface System.Collections.IEnumerable with
        member x.GetEnumerator () = x.GetEnumerator () :> _

    interface INotifyCollectionChanged with
        [<CLIEvent>]
        member x.CollectionChanged = x.CollectionChanged

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = x.PropertyChanged


type 'a collectionStateChangeNotifier () =
    let subscribed = List<ICollectionSubscriber<'a>> ()
    let propChangedEvent = Event<PropertyChangedEventHandler, _> ()
    let collectionChangedEvent = Event<NotifyCollectionChangedEventHandler, _> ()

    member x.Notify sender e =
        subscribed |> Seq.iter (fun u -> u.Update e)
        if e.CountChanged then propChangedEvent.Trigger (sender, PropertyChangedEventArgs "Count")
        if e.ItemsChanged then propChangedEvent.Trigger (sender, PropertyChangedEventArgs "Item[]")
        collectionChangedEvent.Trigger (sender, e.ToNotifyCollectionChangedEventArgs ())

    [<CLIEvent>]
    member x.CollectionChanged = collectionChangedEvent.Publish
    [<CLIEvent>]
    member x.PropertyChanged = propChangedEvent.Publish

    member x.Subscribe u =
        subscribed.Add u

    member x.Unsubscribe u =
        let index = subscribed.IndexOf u
        if index < 0 then
            raise (KeyNotFoundException ())
        subscribed.RemoveAt index


type 'a collectionState when 'a : equality internal (values : seq<'a>) =
    inherit ('a collectionStateBase) ()

    let notifier = collectionStateChangeNotifier ()
    let buffer = List<'a> (values)

    let subBuffer (start : int) count =
        ArraySeg.init count (fun i -> buffer.[i + start])

    [<CLIEvent>]
    override x.CollectionChanged = notifier.CollectionChanged
    [<CLIEvent>]
    override x.PropertyChanged = notifier.PropertyChanged

    override x.Item with get i = buffer.[i]
    override x.Count = buffer.Count

    override x.Subscribe u = notifier.Subscribe u
    override x.Unsubscribe u = notifier.Unsubscribe u

    member private x.ClearItems () =
        let removedItems = subBuffer 0 buffer.Count
        buffer.Clear ()
        notifier.Notify x (RemoveItems (removedItems, 0))

    member private x.InsertItem index item =
        buffer.Insert (index, item)
        notifier.Notify x (AddItems (ArraySeg.singleton item, index))

    member private x.InsertItems index items =
        let items = ArraySeg.ofSeq items
        buffer.InsertRange (index, ArraySeg.toSeq items)
        notifier.Notify x (AddItems (items, index))

    member private x.MoveItem oldIndex newIndex =
        let item = buffer.[oldIndex]
        buffer.RemoveAt oldIndex
        buffer.Insert (newIndex, item)
        notifier.Notify x (MoveItems (ArraySeg.singleton item, oldIndex, newIndex))

    member private x.MoveItems oldIndex newIndex count =
        let items = subBuffer oldIndex count
        buffer.RemoveRange (oldIndex, count)
        buffer.InsertRange (newIndex, ArraySeg.toSeq items)
        notifier.Notify x (MoveItems (items, oldIndex, newIndex))

    member private x.RemoveItem index =
        let item = buffer.[index]
        buffer.RemoveAt index
        notifier.Notify x (RemoveItems (ArraySeg.singleton item, index))

    member private x.RemoveItems index count =
        let items = subBuffer index count
        buffer.RemoveRange (index, count)
        notifier.Notify x (RemoveItems (items, index))

    member private x.ReplaceItem index item =
        let oldItem = buffer.[index]
        buffer.[index] <- item
        notifier.Notify x (ReplaceItems (ArraySeg.singleton oldItem, ArraySeg.singleton item, index))

    member private x.ReplaceItems index items =
        let newItems = ArraySeg.ofSeq items
        let oldItems = subBuffer index newItems.Length
        for i in 0 .. newItems.Length - 1 do
            buffer.[i + index] <- newItems.[i]
        notifier.Notify x (ReplaceItems (oldItems, newItems, index))

    member x.Add item = x.InsertItem buffer.Count item
    member x.AddRange items = x.InsertItems buffer.Count items
    member x.Clear () = x.ClearItems ()
    member x.RemoveAt index = x.RemoveItem index
    member x.RemoveRange index count = x.RemoveItems index count
    member x.Insert index item = x.InsertItem index item
    member x.InsertRange index items = x.InsertItems index items
    member x.Move oldIndex newIndex = x.MoveItem oldIndex newIndex
    member x.MoveRange oldIndex newIndex count = x.MoveItems oldIndex newIndex count
    member x.Replace index item = x.ReplaceItem index item
    member x.ReplaceRange index items = x.ReplaceItems index items

    member x.Set index value = x.ReplaceItem index value

    member x.Remove item =
        let i = buffer.IndexOf item
        if i < 0 then false
        else
            x.RemoveItem i
            true

    member x.ForceRemove item =
        let i = buffer.IndexOf item
        if i < 0 then raise (KeyNotFoundException ())
        x.RemoveItem i

    member x.RemoveAll predicate =
        x
        |> Seq.mapi (fun i item -> item, i)
        |> Seq.filter (fun (item, i) -> predicate item)
        |> Seq.map (fun (item, i) -> i)
        |> Seq.distinct
        |> Seq.sortDescending
        |> Seq.iter (fun i -> x.RemoveItem i)

    override x.GetEnumerator () = buffer.GetEnumerator () :> _

    override x.Contains item = buffer.Contains item
    override x.IndexOf item = buffer.IndexOf item

    interface IReorderableCollection with
        member x.Move oldIndex newIndex = x.Move oldIndex newIndex
        member x.MoveRange oldIndex newIndex count = x.MoveRange oldIndex newIndex count

    interface IList<'a> with
        member x.IndexOf item = x.IndexOf item
        member x.Insert (index, item) = x.Insert index item
        member x.Item
            with get index = x.[index]
            and set index v = x.Set index v
        member x.RemoveAt index = x.RemoveAt index

    interface System.Collections.IList with
        member x.Add value =
            x.Add (unbox value)
            x.Count - 1
        member x.Clear () = x.Clear ()
        member x.Contains value = (buffer :> System.Collections.IList).Contains value
        member x.IndexOf value = (buffer :> System.Collections.IList).IndexOf value
        member x.Insert (index, value) = x.Insert index (unbox value)
        member x.IsFixedSize = false
        member x.IsReadOnly = false
        member x.Item
            with get index = box x.[index]
            and set index v = x.Set index (unbox v)
        member x.Remove value =
            let i = (buffer :> System.Collections.IList).IndexOf value
            if i >= 0 then x.RemoveItem i
        member x.RemoveAt index = x.RemoveAt index

    interface ICollection<'a> with
        member x.Add item = x.Add item
        member x.Clear () = x.Clear ()
        member x.Contains item = x.Contains item
        member x.CopyTo (array, arrayIndex) = buffer.CopyTo (array, arrayIndex)
        member x.Count = x.Count
        member x.IsReadOnly = false
        member x.Remove item = x.Remove item

    interface System.Collections.ICollection with
        member x.CopyTo (array, index) = (buffer :> System.Collections.ICollection).CopyTo (array, index)
        member x.Count = x.Count
        member x.IsSynchronized = false
        member x.SyncRoot = (buffer :> System.Collections.ICollection).SyncRoot

    interface IEnumerable<'a> with
        member x.GetEnumerator () = x.GetEnumerator ()

    interface System.Collections.IEnumerable with
        member x.GetEnumerator () = x.GetEnumerator () :> _


type internal 'a mappedState when 'a : equality (initialValue, getValue, dependencies) as x =
    inherit ('a stateBase) ()

    let inner : 'a state = state (initialValue, Some (box x))

    let asSubscriber =
        { new IStateSubscriber with
            member __.Lock () =
                inner.Lock () |> ignore

            member __.Unlock isUpdateRequested =
                if isUpdateRequested then
                    inner.RequestSetValue getValue
                inner.Unlock () }

    do dependencies
        |> Seq.iter (fun (parent : IParentState) ->
            parent.Subscribe asSubscriber)

    [<CLIEvent>]
    override x.PropertyChanged = inner.PropertyChanged

    override x.Value = inner.Value

    override x.Subscribe child = inner.Subscribe child
    override x.Unsubscribe child = inner.Unsubscribe child

    member x.LockCount = inner.LockCount


type internal 'a readonlyCollectionState (dependency : 'a collectionStateBase) as x =
    inherit ('a collectionStateBase) ()

    let subscribed = List<ICollectionSubscriber<'a>> ()

    let propChangedEvent = Event<PropertyChangedEventHandler, _> ()
    let collectionChangedEvent = Event<_, _> ()

    do dependency.Subscribe (
        { new ICollectionSubscriber<'a> with
            member __.Update arg =
                subscribed |> Seq.iter (fun u -> u.Update arg) })

    do dependency.PropertyChanged.Add (fun e ->
        propChangedEvent.Trigger (x, e))

    do dependency.CollectionChanged.Add (fun e ->
        collectionChangedEvent.Trigger (x, e))

    [<CLIEvent>]
    override x.CollectionChanged = collectionChangedEvent.Publish
    [<CLIEvent>]
    override x.PropertyChanged = propChangedEvent.Publish

    override x.Item with get i = dependency.[i]
    override x.Count = dependency.Count

    override x.Subscribe u =
        subscribed.Add u

    override x.Unsubscribe u =
        let index = subscribed.IndexOf u
        if index < 0 then
            raise (KeyNotFoundException ())
        subscribed.RemoveAt index

    override x.GetEnumerator () = dependency.GetEnumerator ()

    override x.Contains item = dependency.Contains item
    override x.IndexOf item = dependency.IndexOf item


type internal ('i, 's, 'a) collectionMappedState when 'i : equality and 'a : equality (stateMap : 'i -> 's stateBase, getValue : int -> seq<'s> -> 'a, dependency : 'i collectionStateBase) as x =
    inherit ('a stateBase) ()

    let getValue () = getValue dependency.Count (dependency |> Seq.map (fun i -> (stateMap i : 's stateBase).Value))

    let inner : 'a state = state (getValue (), Some (box x))

    let asSubscriber =
        { new IStateSubscriber with
            member __.Lock () =
                inner.Lock () |> ignore

            member __.Unlock isUpdateRequested =
                if isUpdateRequested then
                    inner.RequestSetValue getValue
                inner.Unlock () }

    let collectionSubscriber =
        { new ICollectionSubscriber<_> with
            member __.Update change =
                match change with
                | AddItems (items, index) ->
                    items |> ArraySeg.iter (fun i -> (stateMap i).Subscribe asSubscriber)
                | MoveItems (items, oldIndex, newIndex) -> ()
                | RemoveItems (items, index) ->
                    items |> ArraySeg.iter (fun i -> (stateMap i).Unsubscribe asSubscriber)
                | ReplaceItems (oldItems, newItems, index) ->
                    oldItems |> ArraySeg.iter (fun i -> (stateMap i).Unsubscribe asSubscriber)
                    newItems |> ArraySeg.iter (fun i -> (stateMap i).Subscribe asSubscriber)
                asSubscriber.ForceUpdate () }

    do dependency.Subscribe collectionSubscriber
    do dependency |> Seq.iter (fun i -> (stateMap i).Subscribe asSubscriber)

    [<CLIEvent>]
    override x.PropertyChanged = inner.PropertyChanged

    override x.Value = inner.Value

    override x.Subscribe u = inner.Subscribe u
    override x.Unsubscribe u = inner.Unsubscribe u


type internal ('a, 'i) collectionCountState when 'a : equality (mapper : int -> 'a, collection : 'i collectionStateBase) as x =
    inherit ('a stateBase) ()

    let getValue () = mapper collection.Count

    let inner : 'a state = state (getValue (), Some (box x))

    let updatable =
        { new ICollectionSubscriber<_> with
            member __.Update change =
                inner.RequestSetValue getValue }

    do collection.Subscribe updatable

    [<CLIEvent>]
    override x.PropertyChanged = inner.PropertyChanged

    override x.Value = mapper collection.Count

    override x.Subscribe u = inner.Subscribe u
    override x.Unsubscribe u = inner.Unsubscribe u


type internal 'a compositeCollectionState (c1 : 'a collectionStateBase, c2 : 'a collectionStateBase) as x =
    inherit ('a collectionStateBase) ()

    let subscribed = List<ICollectionSubscriber<'a>> ()

    let propChangedEvent = Event<PropertyChangedEventHandler, _> ()
    let collectionChangedEvent = Event<_, _> ()

    let updatable =
        { new ICollectionSubscriber<'a> with
            member __.Update arg =
                subscribed |> Seq.iter (fun u -> u.Update arg) }
    do c1.Subscribe updatable
    do c2.Subscribe updatable

    do c1.PropertyChanged.Add (fun e ->
        propChangedEvent.Trigger (x, e))
    do c2.PropertyChanged.Add (fun e ->
        propChangedEvent.Trigger (x, e))

    do c1.CollectionChanged.Add (fun e ->
        collectionChangedEvent.Trigger (x, e))
    do c2.CollectionChanged.Add (fun e ->
        let e =
            match e.Action with
            | NotifyCollectionChangedAction.Reset   -> e
            | NotifyCollectionChangedAction.Add     -> NotifyCollectionChangedEventArgs (NotifyCollectionChangedAction.Add,     e.NewItems, e.NewStartingIndex + c1.Count)
            | NotifyCollectionChangedAction.Remove  -> NotifyCollectionChangedEventArgs (NotifyCollectionChangedAction.Remove,  e.OldItems, e.OldStartingIndex + c1.Count)
            | NotifyCollectionChangedAction.Move    -> NotifyCollectionChangedEventArgs (NotifyCollectionChangedAction.Move,    e.OldItems, e.NewStartingIndex + c1.Count, e.OldStartingIndex + c1.Count)
            | NotifyCollectionChangedAction.Replace -> NotifyCollectionChangedEventArgs (NotifyCollectionChangedAction.Replace, e.NewItems, e.OldItems, e.OldStartingIndex + c1.Count)
            | _ -> e
        collectionChangedEvent.Trigger (x, e))

    [<CLIEvent>]
    override x.CollectionChanged = collectionChangedEvent.Publish
    [<CLIEvent>]
    override x.PropertyChanged = propChangedEvent.Publish

    override x.Item with get i = if i < c1.Count then c1.[i] else c2.[i]
    override x.Count = c1.Count + c2.Count

    override x.Subscribe u =
        subscribed.Add u

    override x.Unsubscribe u =
        let index = subscribed.IndexOf u
        if index < 0 then
            raise (KeyNotFoundException ())
        subscribed.RemoveAt index

    override x.GetEnumerator () = (Seq.append c1 c2).GetEnumerator ()

    override x.Contains item = c1.Contains item || c2.Contains item
    override x.IndexOf item =
        let i2 = c2.IndexOf item
        if i2 >= 0 then
            i2 + c1.Count
        else
            c1.IndexOf item


module St =
    let inline private (!!) (s : #_ stateBase) =
        s.Value

    let inline get (s : #_ stateBase) =
        s.Value

    let inline set value (s : _ state) =
        s.SetValue value

    let modify modifier (s : _ state) =
        s.SetValue (modifier s.Value)

    let create value =
        state (value, None)

    let createWpfWritable value onValueChanged =
        stateWpfWritable (value, onValueChanged)

    let createWpfWritablePaired value =
        let st = state (value, None)
        let stwpf = stateWpfWritable (value, (fun v -> st.SetValue v))
        st.Subscribe (StateSubscriber.create (fun () -> stwpf.SetValueSilent st.Value))
        st, stwpf

    let createImmutable value =
        { new (_ stateBase) () with
            member __.Subscribe _ = ()
            member __.Unsubscribe _ = ()
            [<CLIEvent>] member __.PropertyChanged = Event.empty
            member __.Value = value }

    let inline internal mapMultiInternal getValue dependencies =
        mappedState (getValue (), getValue, dependencies) :> _ stateBase

    let mapd initValue mapper state =
        mappedState (initValue, (fun () -> mapper !!state), [| state |]) :> _ stateBase

    let map mapper state =
        mapMultiInternal (fun () -> mapper !!state) [| state |]

    let map2 mapper (s1, s2) =
        mapMultiInternal (fun () -> mapper !!s1 !!s2) [| s1; s2 |]

    let map3 mapper (s1, s2, s3) =
        mapMultiInternal (fun () -> mapper !!s1 !!s2 !!s3) [| s1; s2; s3 |]

    let map4 mapper (s1, s2, s3, s4) =
        mapMultiInternal (fun () -> mapper !!s1 !!s2 !!s3 !!s4) [| s1; s2; s3; s4 |]

    let map5 mapper (s1, s2, s3, s4, s5) =
        mapMultiInternal (fun () -> mapper !!s1 !!s2 !!s3 !!s4 !!s5) [| s1; s2; s3; s4; s5 |]

    let map6 mapper (s1, s2, s3, s4, s5, s6) =
        mapMultiInternal (fun () -> mapper !!s1 !!s2 !!s3 !!s4 !!s5 !!s6) [| s1; s2; s3; s4; s5; s6 |]

    let map7 mapper (s1, s2, s3, s4, s5, s6, s7) =
        mapMultiInternal (fun () -> mapper !!s1 !!s2 !!s3 !!s4 !!s5 !!s6 !!s7) [| s1; s2; s3; s4; s5; s6; s7 |]

    let map8 mapper (s1, s2, s3, s4, s5, s6, s7, s8) =
        mapMultiInternal (fun () -> mapper !!s1 !!s2 !!s3 !!s4 !!s5 !!s6 !!s7 !!s8) [| s1; s2; s3; s4; s5; s6; s7; s8 |]

    let map9 mapper (s1, s2, s3, s4, s5, s6, s7, s8, s9) =
        mapMultiInternal (fun () -> mapper !!s1 !!s2 !!s3 !!s4 !!s5 !!s6 !!s7 !!s8 !!s9) [| s1; s2; s3; s4; s5; s6; s7; s8; s9 |]

    let map10 mapper (s1, s2, s3, s4, s5, s6, s7, s8, s9, s10) =
        mapMultiInternal (fun () -> mapper !!s1 !!s2 !!s3 !!s4 !!s5 !!s6 !!s7 !!s8 !!s9 !!s10) [| s1; s2; s3; s4; s5; s6; s7; s8; s9; s10 |]

    let map11 mapper (s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11) =
        mapMultiInternal (fun () -> mapper !!s1 !!s2 !!s3 !!s4 !!s5 !!s6 !!s7 !!s8 !!s9 !!s10 !!s11) [| s1; s2; s3; s4; s5; s6; s7; s8; s9; s10; s11 |]

    let map12 mapper (s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12) =
        mapMultiInternal (fun () -> mapper !!s1 !!s2 !!s3 !!s4 !!s5 !!s6 !!s7 !!s8 !!s9 !!s10 !!s11 !!s12) [| s1; s2; s3; s4; s5; s6; s7; s8; s9; s10; s11; s12 |]

    let mapSeq getValue (dependencies : seq<#IParentState>) =
        mapMultiInternal getValue (dependencies |> Seq.map (fun s -> s :> IParentState) |> Array.ofSeq)

    let mapArray (dependencies : IParentState []) getValue =
        mapMultiInternal getValue dependencies

    let mapc stateSelector mapper (sc : _ collectionStateBase) =
        collectionMappedState (stateSelector, mapper, sc) :> _ stateBase

    let mapcCount mapper (sc : _ collectionStateBase) =
        collectionCountState (mapper, sc) :> _ stateBase


module Stc =
    let create values =
        collectionState values

    let readonly sc =
        readonlyCollectionState sc :> _ collectionStateBase

    let composite c1 c2 =
        compositeCollectionState (c1, c2) :> _ collectionStateBase

    let map mapper (collection : _ collectionStateBase) =
        let buffer = collectionState (Seq.map mapper collection)

        collection.Subscribe
            { new ICollectionSubscriber<_> with
                member __.Update arg =
                    match arg with
                    | AddItems (items, index) ->
                        let bufferItems = ArraySeg.toSeq (ArraySeg.mapLazy mapper items)
                        buffer.InsertRange index bufferItems
                    | MoveItems (items, oldIndex, newIndex) ->
                        buffer.MoveRange oldIndex newIndex items.Length
                    | RemoveItems (items, index) ->
                        buffer.RemoveRange index items.Length
                    | ReplaceItems (oldItems, newItems, index) ->
                        let bufferNewItems = ArraySeg.toSeq (ArraySeg.mapLazy mapper newItems)
                        buffer.ReplaceRange index bufferNewItems }

        readonlyCollectionState buffer :> _ collectionStateBase

    let collect collector (collection : _ collectionStateBase) =
        let buffer = collectionState (Seq.collect collector collection)
        let toBufferIndex index =
            Seq.sumBy (fun i -> Seq.length (collector collection.[i])) (seq { 0 .. index - 1 })
        let getBufferItems items =
            Seq.collect collector (ArraySeg.toSeq items)
        let countBufferItems items =
            Seq.sumBy (fun item -> Seq.length (collector item)) (ArraySeg.toSeq items)

        collection.Subscribe
            { new ICollectionSubscriber<_> with
                member __.Update arg =
                    match arg with
                    | AddItems (items, index) ->
                        let bufferItems = getBufferItems items
                        let bufferIndex = toBufferIndex index
                        buffer.InsertRange bufferIndex bufferItems
                    | MoveItems (items, oldIndex, newIndex) ->
                        let bufferItemsCount = countBufferItems items
                        let bufferOldIndex =
                            if oldIndex <= newIndex then
                                toBufferIndex oldIndex
                            else
                                toBufferIndex (oldIndex + items.Length) - bufferItemsCount
                        let bufferNewIndex = toBufferIndex newIndex
                        buffer.MoveRange bufferOldIndex bufferNewIndex bufferItemsCount
                    | RemoveItems (items, index) ->
                        let bufferItemsCount = countBufferItems items
                        let bufferIndex = toBufferIndex index
                        buffer.RemoveRange bufferIndex bufferItemsCount
                    | ReplaceItems (oldItems, newItems, index) ->
                        let bufferIndex = toBufferIndex index
                        let bufferOldItemsCount = countBufferItems oldItems
                        let bufferNewItems = getBufferItems newItems
                        if bufferOldItemsCount = Seq.length bufferNewItems then
                            buffer.ReplaceRange bufferIndex bufferNewItems
                        else
                            buffer.RemoveRange bufferIndex bufferOldItemsCount
                            buffer.InsertRange bufferIndex bufferNewItems }

        readonlyCollectionState buffer :> _ collectionStateBase

    let toReadOnlyList (collection : _ collectionStateBase) =
        { new IReadOnlyList<'a> with
            member x.Item
                with get index = collection.[index]
        interface IReadOnlyCollection<'a> with
            member x.Count = collection.Count
        interface IEnumerable<'a> with
            member x.GetEnumerator () =
                collection.GetEnumerator ()
        interface System.Collections.IEnumerable with
            member x.GetEnumerator () =
                collection.GetEnumerator () :> _ }


[<AutoOpen>]
module StUtil =
    let inline (!!) (s : #_ stateBase) =
        St.get s

    let inline st value =
        St.create value

    let inline stwpf value onValueChanged =
        St.createWpfWritable value onValueChanged

    let inline stwpfp value =
        St.createWpfWritablePaired value

    let inline stc values =
        Stc.create values


