namespace DcRioEdit.Models

open System


type IntRange (startValue, endValue) =
    do  if startValue >= endValue then
            raise (ArgumentException ("startValue >= endValue"))

    member x.Start : int = startValue
    member x.End : int = endValue

    member x.Length = endValue - startValue

    override x.ToString () =
        sprintf "[%i, %i)" x.Start x.End

    static member ofLength start length =
        IntRange (start, start + length)

    static member unitLength start =
        IntRange (start, start + 1)

    static member ofStartEnd (startValue, endValue) =
        IntRange (startValue, endValue)

    static member ofValues v1 v2 =
        IntRange (min v1 v2, max v1 v2)

    static member ofStartEndChecked (startValue, endValue) =
        if startValue >= endValue then
            None
        else
            Some (IntRange (startValue, endValue))

    static member ofValuesChecked v1 v2 =
        if v1 = v2 then
            None
        else
            Some (IntRange (min v1 v2, max v1 v2))

    member x.Union (y : IntRange) =
        IntRange (min x.Start y.Start, max x.End y.End)

    member x.Intersect (y : IntRange) =
        IntRange.ofStartEndChecked (max x.Start y.Start, min x.End y.End)

    member x.Except (y : IntRange) =
        IntRange.ofStartEndChecked (x.Start, y.Start),
        IntRange.ofStartEndChecked (y.End, x.End)

    member x.Xor (y : IntRange) =
        IntRange.ofValuesChecked x.Start y.Start,
        IntRange.ofValuesChecked x.End y.End

    member x.Contains value =
        value >= x.Start && value < x.End

    static member inline union y (x : IntRange) = x.Union y
    static member inline intersect y (x : IntRange) = x.Intersect y
    static member inline except y (x : IntRange) = x.Except y
    static member inline xor y (x : IntRange) = x.Xor y

