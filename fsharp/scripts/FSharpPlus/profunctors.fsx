#r "nuget: FSharpPlus"

open FSharpPlus.Operators

let toNegative = (*) -1
let length (s: string) = s.Length
let by30 = (*) 30
let d = dimap length by30 toNegative
let d' (s: string) = s |> length |> toNegative |> by30

let transform =
    dimap
        (fun input -> -input)
        (fun output -> output + 10)
        (fun x -> x * 2)

let customTransform (fn: int -> int) =
    dimap
        (fun input -> -input)
        (fun output -> output + 10)
        fn
// transform' ((*) 3) 5;;
// val it: int = -5

let modifyStr =
    dimap
        (fun (input: string) -> System.Text.StringBuilder input)
        (fun output -> output.ToString())
        (fun x -> x.Append "-MODIFIED")
