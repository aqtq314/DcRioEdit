namespace DcRioEdit.Models

open System


[<AutoOpen>]
module Math =
    let pi = Math.PI

    let inline lerp (a : ^a) (b : ^a) amount : ^a =
        amount * (b - a) + a

    let inline unlerp (a : ^a) (b : ^a) (value : ^a) =
        (value - a) / (b - a)

    let inline half x = x / 2.0

    let inline sqr v = v * v

    let inline cube v = v * v * v

    let inline inv v = 1.0 / v

    let inline clamp minValue maxValue value = value |> min maxValue |> max minValue

    let inline between minValue maxValue value = value >= minValue && value < maxValue

    let inline invlerp (a : ^a) (b : ^a) (c : ^a) amount : ^a =
        let one = LanguagePrimitives.GenericOne
        let y = b |> unlerp a c
        let k = sqrt ((one - y) / y)
        let result = amount / (k * k * (one - amount) + amount)
        result |> lerp a c

    let inline toDeg angle = angle * 180.0 / pi

    let inline angleOfDeg angleDeg = angleDeg * pi / 180.0

    let round unitValue (value : float) =
        Math.Round (value / unitValue) * unitValue

    let normalizeAngle value =
        if Double.IsNaN value || Double.IsInfinity value then
            0.0
        else
            let n = value % (pi * 2.0)
            if n <= -pi then
                n + (pi * 2.0)
            elif n > pi then
                n - (pi * 2.0)
            else n

    type [<RequireQualifiedAccess>] Quadrant =
        | Q1 | Q2 | Q3 | Q4 | XPos | XNeg | YPos | YNeg

        static member ofAngle angle =
            let angle = normalizeAngle angle
            if   angle < -pi / 2.0 then Quadrant.Q3
            elif angle = -pi / 2.0 then Quadrant.YNeg
            elif angle < 0.0       then Quadrant.Q4
            elif angle = 0.0       then Quadrant.XPos
            elif angle < pi / 2.0  then Quadrant.Q1
            elif angle = pi / 2.0  then Quadrant.YPos
            elif angle < pi        then Quadrant.Q2
            else                        Quadrant.XNeg


type VectorF =
    | VectorF of x : float * y : float

    override x.ToString () =
        let (VectorF (x, y)) = x
        sprintf "<%A, %A>" x y

    member x.X = let (VectorF (x, y)) = x in x
    member x.Y = let (VectorF (x, y)) = x in y

    member x.Magnitude =
        let (VectorF (x, y)) = x
        sqrt (x * x + y * y)

    member x.MagnitudeSquared =
        let (VectorF (x, y)) = x
        x * x + y * y

    member x.Angle =
        let (VectorF (x, y)) = x
        atan2 y x

    member x.AngleInDegrees =
        x.Angle * 180.0 / pi

    member x.Scale (scaleX, scaleY) =
        let (VectorF (x, y)) = x
        VectorF (x * scaleX, y * scaleY)

    member x.Rotate angle =
        let (VectorF (x, y)) = x
        let cosang = cos angle
        let sinang = sin angle
        VectorF (
            x * cosang - y * sinang,
            x * sinang + y * cosang)

    member x.Rotate90 () =
        let (VectorF (x, y)) = x
        VectorF (-y, x)

    member x.RotateNeg90 () =
        let (VectorF (x, y)) = x
        VectorF (y, -x)

    member x.Multiply k =
        let (VectorF (x, y)) = x
        VectorF (x * k, y * k)

    member x.Divide k =
        let (VectorF (x, y)) = x
        VectorF (x / k, y / k)

    member x.Normalized =
        let mag = x.Magnitude
        let (VectorF (x, y)) = x
        VectorF (x / mag, y / mag)

    static member (+) (u, v) =
        let (VectorF (ux, uy)) = u
        let (VectorF (vx, vy)) = v
        VectorF (ux + vx, uy + vy)

    static member (-) (u, v) =
        let (VectorF (ux, uy)) = u
        let (VectorF (vx, vy)) = v
        VectorF (ux - vx, uy - vy)

    static member (~-) u =
        let (VectorF (ux, uy)) = u
        VectorF (-ux, -uy)

    static member (*) (u : VectorF, k) =
        u.Multiply k

    static member (*) (k, u : VectorF) =
        u.Multiply k

    static member (/) (u : VectorF, k) =
        u.Divide k

    static member ( *. ) (u, v) =
        let (VectorF (ux, uy)) = u
        let (VectorF (vx, vy)) = v
        ux * vx + uy * vy

    static member ( *! ) (u, v) =
        let (VectorF (ux, uy)) = u
        let (VectorF (vx, vy)) = v
        ux * vy - uy * vx

    static member zero = VectorF (0.0, 0.0)

    static member ofPolar (angle, mag) =
        VectorF (mag * cos angle, mag * sin angle)

    static member inline xOf (v : VectorF) = v.X
    static member inline yOf (v : VectorF) = v.Y

    static member inline dot (u : VectorF) (v : VectorF) = u *. v
    static member inline dotSelf (u : VectorF) = u.MagnitudeSquared

    static member inline angleOf (v : VectorF) = v.Angle
    static member inline magOf (v : VectorF) = v.Magnitude
    static member inline magSquaredOf (v : VectorF) = v.MagnitudeSquared

    static member inline scale (scaleX, scaleY) (v : VectorF) = v.Scale (scaleX, scaleY)
    static member inline rotate angle (v : VectorF) = v.Rotate angle
    static member inline rotate90 (v : VectorF) = v.Rotate90 ()
    static member inline rotateNeg90 (v : VectorF) = v.RotateNeg90 ()
    static member inline normalize (v : VectorF) = v.Normalized

    static member elementwiseDiv (u : VectorF) (v : VectorF) = u.X / v.X, u.Y / v.Y

    static member proj (onto : VectorF) (v : VectorF) = onto * ((v *. onto) / onto.MagnitudeSquared)

    static member angleBetween (u : VectorF) (v : VectorF) = acos (u *. v / u.Magnitude / v.Magnitude)

    static member intersection p1 p2 q1 q2 =
        let (VectorF (p1X, p1Y)) = p1
        let (VectorF (p2X, p2Y)) = p2
        let (VectorF (q1X, q1Y)) = q1
        let (VectorF (q2X, q2Y)) = q2
        let d1 = p1X * p2Y - p1Y * p2X
        let d2 = q1X * q2Y - q1Y * q2X
        let denom = (p1X - p2X) * (q1Y - q2Y) - (p1Y - p2Y) * (q1X - q2X)
        let rx = (d1 * (q1X - q2X) - (p1X - p2X) * d2) / denom
        let ry = (d1 * (q1Y - q2Y) - (p1Y - p2Y) * d2) / denom
        VectorF (rx, ry)

    static member lerp (v0 : VectorF) (v1 : VectorF) t =
        lerp v0 v1 t

    static member qerp (v0 : VectorF) (v1 : VectorF) (v2 : VectorF) t =
        1.0 * (
                    t * (2.0 * t - 1.0)             * v2
            - 4.0 * t                   * (t - 1.0) * v1
            +           (2.0 * t - 1.0) * (t - 1.0) * v0)

    static member cerp (v0 : VectorF) (v1 : VectorF) (v2 : VectorF) (v3 : VectorF) t =
        1.0 / 2.0 * (
                    t * (3.0 * t - 1.0) * (3.0 * t - 2.0)             * v3
            - 9.0 * t * (3.0 * t - 1.0)                   * (t - 1.0) * v2
            + 9.0 * t                   * (3.0 * t - 2.0) * (t - 1.0) * v1
            -           (3.0 * t - 1.0) * (3.0 * t - 2.0) * (t - 1.0) * v0)        


[<AutoOpen>]
module VectorExt =
    let inline vec x y = VectorF (float x, float y)
    let inline vecu x = VectorF (float x, float x)
    let inline vecp angle mag = VectorF.ofPolar (float angle, float mag)
    let inline unvec (v : VectorF) = v.X, v.Y
    let vecz = VectorF.zero


