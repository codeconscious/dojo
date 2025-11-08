#r "nuget:  FSharpPlus"

open System
open System.Text
open FSharpPlus

let printEach (xs: 'a seq) =
    xs |> Seq.map _.ToString() |> String.concat "; " |> printf "%A"
    printfn "%s" String.Empty

printEach <|    Seq.apply [(*) 2; (*) 3] [1..5]
printEach <|    Seq.bind (fun x -> seq [x; x*2; x*3]) [1..4]
printEach <|    Seq.choosei (fun i o -> if i < 5 then Some (o * 3) else None) [1..10]
printfn "%A" <| Seq.chunkBy (fun (x: string) -> Char.ToUpperInvariant x[0]) ["apple"; "ant"; "bat"; "cat"; "carrot"]
// printfn "%A" <| Seq.groupBy (fun (x: string) -> Char.ToUpperInvariant x[0]) ["apple"; "ant"; "bat"; "cat"; "carrot"]
printEach <|    Seq.drop 5 [1..15]
printfn "%d" <| Seq.findSliceIndex [9..11] [1..12] // See also: tryFindSliceIndex
printfn "%s"   (Seq.foldBack (fun (x: char) (s: StringBuilder) -> s.Append x) ['a'..'z'] (StringBuilder()) |> _.ToString())
printEach <|    Seq.intercalate [0] [[1..3]; [10..13]; [20..23]] // "1; 2; 3; 0; 10; 11; 12; 13; 0; 20; 21; 22; 23; 24"
printEach <|    Seq.intersperse 0 [1..10] // "1; 0; 2; 0; 3; 0; 4; 0; 5; 0; 6; 0; 7; 0; 8; 0; 9; 0; 10"
printEach <|    Seq.lift2 (fun x y -> (x + y) * 2) [0..4] [100..104]
printEach <|    Seq.lift3 (fun x y z -> (x + y + z) * 2) [0..2] [100..102] [1000..1002]
printEach <|    Seq.replace [5..6] [333..340] [0..10]
printEach <|    Seq.replicate 10 "あ"
printfn "%A" <| Seq.split [[3]; [7]] [0..11] // seq [[|0; 1; 2|]; [|4; 5; 6|]; seq [8; 9; 10; 11]]
printfn "%A" <| Seq.tryFindSliceIndex [9..11] [1..12] // Some 8

printfn "Done!"
