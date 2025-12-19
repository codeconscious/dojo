#r "nuget: FSharpPlus"

open FSharpPlus
open System

module Tuples =
    let add1 x = x + 1
    let toUpper (s:string) = s.ToUpper()
    let t = 1, "hello"
    let t' = bimap add1 toUpper t // t' = (2, "HELLO")
    first ((+) 10) t'
    let reverseStr (s: string) = s.ToCharArray() |> Array.rev |> String
    second reverseStr t'

    module SingleOnly =
        let t = (1, "a")
        let l = t |> first (fun x -> x + 10)   // (11, "a")
        let r = t |> second (fun s -> s + "!") // (1, "a!")

module Results =
    let mapErr e = sprintf "ERR: %s" e
    let mapOk v = v * 2

    let r1 : Result<int,string> = Ok 21
    let r2 : Result<int,string> = Error "bad"

    let r1' = r1 |> bimap mapErr mapOk // Ok 42
    let r2' = r2 |> bimap mapErr mapOk // Error "ERR: bad"

    module SingleOnly =
        // Error comes first, then OK!
        r1' |> second ((*) 2)
        r2' |> first (fun (x: string) -> x.ToUpper())
        r1 |> bimap id (fun x -> (x + 100).ToString())
