namespace DcRioEdit.Models

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Collections.Specialized
open System.ComponentModel
open System.Windows
open System.Windows.Media


[<AutoOpen>]
module Util =
    let (|>!) a f = f a; a

    let inline (|||>) (a, b, c) f = f a b c

    let rec fix y x = y (fix y) x

    let inline isNotNull value = not (isNull value)


module Option =
    let getOrFallbackTo fallback value =
        match value with
        | Some value -> value
        | None -> fallback

    let getOrGetFallback getFallback value =
        match value with
        | Some value -> value
        | None -> getFallback ()

    let ofByRef (isSuccess, value) =
        if isSuccess then Some value else None


module Lazy =
    let inline force (v : Lazy<_>) =
        v.Force ()

    let inline isValueCreated (v : Lazy<_>) =
        v.IsValueCreated


module String =
    let contains value (s : string) =
        s.Contains value

    let containsChar value (s : string) =
        s |> Seq.contains value

    let toUpper (s : string) =
        s.ToUpper ()

    let toLower (s : string) =
        s.ToLower ()


module KeyValuePair =
    let inline keyOf pair =
        let (KeyValue (key, value)) = pair
        key

    let inline valueOf pair =
        let (KeyValue (key, value)) = pair
        value


module Dict =
    let create (keyValuePairs : seq<KeyValuePair<_, _>>) =
        System.Linq.Enumerable.ToDictionary (
            keyValuePairs,
            (fun (KeyValue (key, value)) -> key),
            (fun (KeyValue (key, value)) -> value))

    let ofSeq (keyValuePairs : seq<_ * _>) =
        System.Linq.Enumerable.ToDictionary (
            keyValuePairs,
            (fun (key, value) -> key),
            (fun (key, value) -> value))

    let find key (table : Dictionary<_, _>) =
        table.[key]

    let tryFind key (table : Dictionary<_, _>) =
        table.TryGetValue key |> Option.ofByRef


module Disposable =
    let empty =
        { new IDisposable with
            member __.Dispose () = () }

    let inline create dispose =
        { new IDisposable with
            member __.Dispose () = dispose () }

    let createOnce dispose =
        let dispose = Lazy.Create dispose
        { new IDisposable with
            member __.Dispose () = (dispose : Lazy<_>).Force () }

    let inline dispose (d : IDisposable) =
        d.Dispose ()


module Event =
    let empty =
        {   new IEvent<'h, 't>
            interface IObservable<'t> with
                member __.Subscribe _ = Disposable.empty
            interface IDelegateEvent<'h> with
                member __.AddHandler _ = ()
                member __.RemoveHandler _ = () }


type OptionBuilder () =
    member x.Bind (m, f) =
        match m with
        | Some m -> f m
        | None -> None

    member x.Bind (m, f) =
        match m with
        | Some m -> f m
        | None -> ()

    member x.Return m = Some m

    member x.ReturnFrom (m : _ option) = m

    member x.Zero () = ()


[<AutoOpen>]
module OptionUtil =
    let option = OptionBuilder ()



