open System
open Library

[<EntryPoint>]
let main args =
    printfn "F#アプリを実行したぜ！"
    printfn "System.Text.Json output regarding args:"

    let value, json = getJson {| args=args; year=System.DateTime.Now.Year |}
    printfn $"Input: %0A{value}"
    printfn $"Output: %s{json}"

    let tens1 = everyTen [0..99]
    printfn "Tens: %A" tens1

    let tens2 = every [0..99] 10
    printfn "Tens: %A" tens2

    let sevens = every [0..99] 7
    printfn "Sevens: %A" sevens

    let sumTo30 = sumTo 30
    printfn "Tuples: %A" sumTo30
    printfn "Tuples summed: %A" (sumTuples sumTo30)

    0
