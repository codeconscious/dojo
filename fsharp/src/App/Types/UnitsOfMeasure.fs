module UnitsOfMeasure // 🤯

[<Measure>] type cm
[<Measure>] type inch
[<Measure>] type m
[<Measure>] type sec
[<Measure>] type kg

let oneCm = 1<cm>
let threeCms = oneCm + 2<cm>

let distance = 1.0<m>
let time = 2.0<sec>
let speed = 2.0<m/sec>
let acceration = 2.0<m/sec^2>
let force = 5.0<kg m/sec^2>

// Derived units of measure
[<Measure>] type N = kg m/sec^2
let force1 = 5.0<kg m/sec^2>
let force2 = 5.0<N>
force1 = force2 |> ignore // true!

// Conversions
[<Measure>] type degC
[<Measure>] type degF

// `<degF/degC>` indicates that the input is `degC` and the output should be `degF` (I believe).
let convertDegCToF c : float<degF> = c * 1.8<degF/degC> + 32.0<degF>
convertDegCToF 0.<degC> |> ignore // 32.0 (float<degF>)

let cmPerMeter = 100.0<cm/m>
let distanceInCm = 2.0<m> * cmPerMeter
