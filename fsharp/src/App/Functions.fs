module Functions

module FunctionComposition =
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

    let f1 i = i + 3
    let f2 i = i * 3
    let f3 i:int = i * i
    let composedF = f1 >> f2 >> f3
    let composedF2 = f3 << f2 << f1

module Folds =
    // Reference: http://www.mikeobrien.net/blog/folding-and-unfolding-in-f-and-linq
    let private lst = [2.0..4.0]
    let folded = List.fold (fun state item -> state ** item) 2. lst
    let foldedBack = List.foldBack (fun state item -> state ** item) lst 2.
    let evenNumbers =
        // Unfold creates a collection.
        Seq.unfold (fun state -> Some (state, 2 + state)) 0
        |> Seq.takeWhile(fun i -> i <= 10)
        |> Seq.map (fun i -> i.ToString())
        |> Seq.reduce (fun state item -> state + ", " + item)

module Reduce =
    // When using`reduce`, the first element is the seed; with `reduceBack`, the last element is the seed.
    let private lst = ["Alpha";"Beta";"Gamma";"Delta"]
    let reduced = lst |> List.reduce (fun state item -> state + ", " + item)
    let reducedBack = lst |> List.reduceBack (fun state item -> state + ", " + item)
