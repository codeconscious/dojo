// Reference: https://aka.ms/fsharp-console-apps
open System
open Library

[<EntryPoint>]
let main args =
    printfn "F#アプラを実行したぜ！"
    printfn "System.Text.Json output regarding args:"

    let value, json = getJson {| args=args; year=System.DateTime.Now.Year |}
    printfn $"Input: %0A{value}"
    printfn $"Output: %s{json}"

    0
