open System
open Library

module Basics =
    // printfn "System.Text.Json output regarding args:"
    let args = [|""|]
    let value, json = getJson {| args=args; year=System.DateTime.Now.Year |}
    // printfn $"Input: %0A{value}"
    // printfn $"Output: %s{json}"

    // printfn "Tens: %A" (everyTen [0..99])
    // printfn "Tens: %A" (every [0..99] 10)
    // printfn "Sevens: %A" (every [0..99] 7)

    let sumTo30 = sumTo 30
    // printfn "Tuples: %A" sumTo30
    // printfn "Tuples summed: %A" (sumTuples sumTo30)

module Functions =
    // Higher-order functions
    let powerLevel = 100
    let addTen i = i + 10
    let applyAndDouble (fn:int -> int) i = fn i * 2
    // printfn "Final: %i" (applyAndDouble addTen powerLevel)

    // Partial application of arguments
    let areBothTrue x y = x && y
    let testPartial = areBothTrue true
    // printfn "%b" (testPartial true)
    // printfn "%b" (testPartial false)

    // Recursive
    let rec fib n = if n < 2 then 1 else fib(n-1) + fib(n-2)
    // printfn "%i" (fib 10)

module FunctionComposition =
    let getRandom(pool:list<string>) =
        let rnd = Random()
        pool
        |> List.map(fun x -> x.Replace('は', 'ズ'))
        |> List.item(rnd.Next(pool.Length))
    let encrypt(word:string) =
        word |> String.map(fun s -> '*')
    let randomEncrypted = getRandom >> encrypt

module People =
    open MyTypes

    let scott = Person("Scott", "Summers", (uint8) 33)
    let cyclops = SuperPerson(
        "Cyclops",
        Mutant,
        Powers([|"optic blasts"|]),
        scott)
    let jean = Person("Jean", "Grey-Summers", (uint8) 32)
    let phoenix = SuperPerson(
        "Phoenix",
        Mutant,
        Powers([|"telepathy"; "telekinesis"|]),
        jean)
    let ororo = Person("Ororo", "Munroe", (uint8) 32)
    let storm = SuperPerson(
        "Storm",
        Mutant,
        Powers([|"weather manipulation"|]),
        ororo)
    // let moira = Person("Moria", "MacTaggert", (uint8) 43)
    let xmen = MyTypes.Team("X-Men", [|cyclops; phoenix; storm|])
    // printfn "%s" xmen.Describe
    // printfn "%s" cyclops.Describe
    // printfn "%s" phoenix.Describe
    // printfn "%s" storm.Describe

// [<EntryPoint>]
// let main args =
//     open FunctionComposition
//     printfn "F#のアプリを実行したぜ！"
//     0

module Run =
    open FunctionComposition
    let words = ["おはようございます"; "こんにちは"; "こんばんは"]
    printfn "Random greeting: %s" (getRandom words)
    printfn "Encrypted word: %s" (encrypt "hello")
    printfn "Random encrypted word: %s" (randomEncrypted words)
