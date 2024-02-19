module Dice

type Die = Die of list<int>

// Original manual ctor:
// let createDie max = Die (Seq.toList { 1 .. max })

module Die = // common idiom: type companion module
    let tryCreate ceiling =
        if ceiling < 1
        then None
        else Some (Die (Seq.toList { 1 .. ceiling }))
    // let values (Die faceValues) = faceValues
    let standard = Die (Seq.toList { 1 .. 6 })

let private rnd = System.Random()
let roll (Die d) = d[rnd.Next(d.Length - 1)]

let rollPrintMultiple times die =
    for i = 1 to times do
        let rolled = roll die
        printfn "You rolled a %i" rolled

let rollPrintOnce = rollPrintMultiple 1
