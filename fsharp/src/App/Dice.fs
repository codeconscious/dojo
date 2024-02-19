module Dice
    type Die = Die of array<int>
    let createDie max =
       Die (Seq.toArray { 1 .. max })

    let rnd = System.Random()
    let roll (Die d) = d[rnd.Next(d.Length - 1)]

    // let rollMultiple (Die d) times =
    //     for i = 1 to times do

    let rollPrintMultiple times die =
        for i = 1 to times do
            let rolled = roll die
            printfn "You rolled a %i" rolled

    let rollPrintOnce = rollPrintMultiple 1
