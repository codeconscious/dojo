module FunctionComposition
    // Sample from https://fsharpforfunandprofit.com/posts/function-composition:
    let private add n x = x + n
    let private times n x = x * n
    let add1Times2 = add 1 >> times 2
    let add5Times3 = add 5 >> times 3

    let private twice f = f >> f
    let private add1 = (+) 1
    let add1Twice = twice add1

    let add1ThenMultiply = (+) 1 >> (*) // もう、すげぇ…
    let times10Add1 = add 1 << times 10

    // Reverse composition (英語っぽくするため、とか)
    // These two lines are equivalent:
    [] |> List.isEmpty |> not |> ignore
    [] |> (not << List.isEmpty) |> ignore
