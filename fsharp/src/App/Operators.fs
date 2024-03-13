module Operators

module PipeForward =
    let private f1 i = i + 3
    let private f2 i = i * 3
    let private f3 i:int = i * i
    let composedF = f1 >> f2 >> f3
    let composedF2 = f3 << f2 << f1

    let pipelined i =
        i
        |> f1
        |> f2
        |> f3

module DoublePipeForward =
    let f1 (a, b) = (a * 2, b * 2)
    let f2 (a, b) = (a * 3, b * 3)
    let f3 (a, b) = (a / 3, b / 3) // Division returns an int

    let composedF1 = f1 >> f2 >> f3
    let composedF2 = f3 << f2 << f1
    let pipelined intPair =
        intPair
        |> f1
        |> f2
        |> f3
