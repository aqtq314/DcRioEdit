namespace DcRioEdit.Models

open System


type Range (startValue, endValue) =
    do  if startValue >= endValue then
            raise (ArgumentException ("startValue >= endValue"))

    member x.Start = startValue
    member x.End = endValue

    member x.Length = endValue - startValue

    override x.ToString () =
        sprintf "[%i, %i)" x.Start x.End

    static member ofLength start length =
        Range (start, start + length)

    static member unitLength start =
        Range (start, start + 1)

    static member ofStartEnd (startValue, endValue) =
        Range (startValue, endValue)

    static member ofValues v1 v2 =
        Range (min v1 v2, max v1 v2)

    static member ofStartEndChecked (startValue, endValue) =
        if startValue >= endValue then
            None
        else
            Some (Range (startValue, endValue))

    static member ofValuesChecked v1 v2 =
        if v1 = v2 then
            None
        else
            Some (Range (min v1 v2, max v1 v2))

    member x.Union (y : Range) =
        Range (min x.Start y.Start, max x.End y.End)

    member x.Intersect (y : Range) =
        Range.ofStartEndChecked (max x.Start y.Start, min x.End y.End)

    member x.Except (y : Range) =
        Range.ofStartEndChecked (x.Start, y.Start),
        Range.ofStartEndChecked (y.End, x.End)

    member x.Xor (y : Range) =
        Range.ofValuesChecked x.Start y.Start,
        Range.ofValuesChecked x.End y.End

    member x.Contains value =
        value >= x.Start && value < x.End

    static member inline union y (x : Range) = x.Union y
    static member inline intersect y (x : Range) = x.Intersect y
    static member inline except y (x : Range) = x.Except y
    static member inline xor y (x : Range) = x.Xor y

