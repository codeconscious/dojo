open System
open Library

[<EntryPoint>]
let main args =
    printfn "F#のアプリを実行したぜ！"
    printfn "System.Text.Json output regarding args:"

    let value, json = getJson {| args=args; year=System.DateTime.Now.Year |}
    printfn $"Input: %0A{value}"
    printfn $"Output: %s{json}"

    printfn "Tens: %A" (everyTen [0..99])
    printfn "Tens: %A" (every [0..99] 10)
    printfn "Sevens: %A" (every [0..99] 7)

    let sumTo30 = sumTo 30
    printfn "Tuples: %A" sumTo30
    printfn "Tuples summed: %A" (sumTuples sumTo30)

    let scott = MyTypes.Person("Scott", "Summers", (uint8) 33)
    let cyclops = MyTypes.SuperPerson("Cyclops", [|"optic blasts"|], MyTypes.Mutant, scott)
    let ororo = MyTypes.Person("Ororo", "Munroe", (uint8) 33)
    let storm = MyTypes.SuperPerson("Store", [|"weather manipulation"|], MyTypes.Mutant, ororo)
    let xmen = MyTypes.Team("X-Men", [|cyclops; storm|])
    printfn "%s" xmen.Describe
    printfn "%s" cyclops.Describe
    printfn "%s" scott.Describe

    0
