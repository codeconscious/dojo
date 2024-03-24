module Recursion

let accWithInternalRecFn items =
    let rec loop acc items =
        match items with
        | [] -> acc
        | head::tail -> loop (acc + head) tail
    loop 0 items

let recFold f acc items =
    let rec loop f acc items =
        match items with
        | [] -> acc
        | head::tail -> loop f (f acc head) tail
    loop f acc items

let rec nonTailRecursiveSum items =
    match items with
    | [] -> 0
    | head::tail -> head + nonTailRecursiveSum tail
nonTailRecursiveSum [1..1000] |> ignore

let rec tailRecursiveSum running_total items =
    match items with
    | [] -> running_total
    | head::tail -> tailRecursiveSum (running_total + head) tail
tailRecursiveSum 0 [1..10000] |> ignore

// https://thinkfunctionally.hashnode.dev/recursive-functions-in-f-sharp
module TailRecursion2 =
    let rec sum list =
        match list with
        | head :: tail -> head + sum tail
        | [] -> 0

    let safeSum list =
        let rec inner l total =
            match l with
            | head :: tail -> inner tail (head + total)
            | [] -> total
        inner list 0

// Source: https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/recursive-functions-the-rec-keyword
module Fibonacci =
    let rec fib n =
        match n with
        | 0 | 1 -> n
        | n -> fib (n-1) + fib (n-2)

    let fibWithTailRecursion n =
        // A recursive inner function named loop, which is an idiomatic F# pattern.
        let rec loop acc1 acc2 n =
            match n with
            | 0 -> acc1
            | 1 -> acc2
            | _ -> loop acc2 (acc1 + acc2) (n-1)
        loop 0 1 n

module MutuallyExclusive =
    let rec Even x = if x = 0 then true else Odd(x - 1)
    and Odd x = if x = 0 then false else Even(x - 1)

module RecursiveValues =
    let rec nameDoubles = nameof nameDoubles + nameof nameDoubles

module Practice =
    let printEachChar (str:string) =
        let chars = str |> Seq.toList
        let rec loop chars =
            match chars with
            | [] -> ()
            | h :: t -> // |> head :: tail
                printf " > %c" h
                loop t
        loop chars
