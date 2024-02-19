module TypeParams

// Source: https://fsprojects.github.io/fsharp-cheatsheet/
type RequestA = { Id: string; StringValue: string }
type RequestB = { Id: string; IntValue: int }
type RequestB2 = { Id2: string; IntValue: int }

let requestA: RequestA = { Id = "A"; StringValue = "Value" }
let requestB: RequestB = { Id = "B"; IntValue = 42 }

let inline getId<'T when 'T : (member Id: string)> (x: 'T) = x.Id
let inline getId2<'T when 'T : (member Id23: string)> (x: 'T) = x.Id23

let idA = getId requestA  // "A"
let idB = getId requestB  // "B"
