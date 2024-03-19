module Blackjack
open Microsoft.FSharp.Reflection

type Suit =
    Hearts | Diamonds | Clubs | Spades
    // Reference: https://stackoverflow.com/a/4470670/11767771
    static member GetSeqValues() =
     // Get all cases of the union
     let cases = FSharpType.GetUnionCases(typeof<Suit>)
     [ for c in cases do
         // Create value for each case (assuming it has no arguments)
         let interest = FSharpValue.MakeUnion(c, [| |]) :?> Suit
         yield interest ]
type Face =
    | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
    | Jack | Queen | King | Ace
    static member GetSeqValues() =
     // Get all cases of the union
     let cases = FSharpType.GetUnionCases(typeof<Face>)
     [ for c in cases do
         // Create value for each case (assuming it has no arguments)
         let interest = FSharpValue.MakeUnion(c, [| |]) :?> Face
         yield interest ]

type Card = { Face: Face; Suit: Suit }

let fullDeck = [
    for suit in Suit.GetSeqValues() do
        for face in Face.GetSeqValues() do
             yield { Face = face; Suit = suit }
    ]
