module Concepts

module SingleCaseDiscrimatedUnions =
    type OrderId = Order of string
    let orderId : OrderId = Order "12"
    let (Order id) = orderId  // id = "12"  // Using pattern matching to deconstruct single-case DU
    printfn "%s" id

module PatternMatchingWithFunction =
    let filterNumbers num =
        match num with
            | 1 | 2 | 3 -> printfn "Found 1, 2, or 3!"
            | a -> printfn "%d" a

    let filterNumbers' =  // the paramater and `match num with` are combined
        function | 1 | 2 | 3 -> printfn "Found 1, 2, or 3!"
                 | a -> printfn "%d" a

    let two = 2
    filterNumbers' two

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

module ActivePatterns =
    // This won't work in pattern matching because the "pattern discriminator ‘countWords’ is not defined":
    // let countWords (document : string) =
    //     document.Split([|' '|]).Length

    // We need an active pattern to resolve it:
    let (|CountWords|) (document : string) = // This is an "active recognizer"
        document.Split([|' '|]).Length

    let documentType document =
        match document with
        | CountWords 5 -> printfn "exactly 5 words"
        | CountWords wc when wc < 20 -> printfn "tweet with %i words" wc
        | CountWords wc when wc < 200 -> printfn "email with %i words" wc
        | CountWords wc when wc < 3000 -> printfn "blog post with %i words" wc
        | CountWords wc -> printfn "novel with %i words" wc
