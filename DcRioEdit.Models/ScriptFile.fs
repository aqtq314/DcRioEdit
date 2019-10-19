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
    let utf8 = Encoding.GetEncoding "utf-8"


type ITranscription =
    abstract Range : Range with get, set

type IScriptFile =
    inherit IEnumerable<ITranscription>
    abstract Content : ArraySeg<byte>
    abstract Item : index : int -> ITranscription with get
    abstract Count : int
    abstract InsertNew : index : int -> range : Range -> ITranscription
    abstract Remove : item : ITranscription -> unit
    abstract RemoveAt : index : int -> unit
    abstract GetMaxRange : item : ITranscription -> Range

[<AutoOpen>]
module ScriptFileUtils =
    module ScriptFile =
        let empty =
            {   new IScriptFile with
                    member x.Content = ArraySeg.empty
                    member x.Item with get index = raise (InvalidOperationException ())
                    member x.Count = 0
                    member x.InsertNew index range = raise (InvalidOperationException ())
                    member x.Remove item = raise (InvalidOperationException ())
                    member x.RemoveAt item = raise (InvalidOperationException ())
                    member x.GetMaxRange item = raise (InvalidOperationException ())
                interface IEnumerable<ITranscription> with
                    member x.GetEnumerator () = Seq.empty.GetEnumerator ()
                interface System.Collections.IEnumerable with
                    member x.GetEnumerator () = (Seq.empty :> System.Collections.IEnumerable).GetEnumerator () }


