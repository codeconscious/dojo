open System

// module Exercise01 =
//     printfn("Hello, World!")

module Exercise02 =
    let expectedMinutesInOven = 40
    let remainingMinutesInOven elapsedMinutes = expectedMinutesInOven - elapsedMinutes
    let preparationTimeInMinutes layerCount = layerCount * 2
    let elapsedTimeInMinutes layerCount minutesInOven = (preparationTimeInMinutes layerCount) + minutesInOven

module Exercise03 =
    let canFastAttack isAwake = isAwake = false
    let canSpy knightIsAwake archerIsAwake prisonerIsAwake = knightIsAwake || archerIsAwake || prisonerIsAwake
    let canSignalPrisoner archerIsAwake prisonerIsAwake = prisonerIsAwake && archerIsAwake = false
    let canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent = (petDogIsPresent && archerIsAwake = false) || (prisonerIsAwake && knightIsAwake = false && archerIsAwake = false)

module StringsLogLevels =
    let message (str: string) = "[ERROR]: " + str.Trim()

module Run =
    // open Exercise02
    // printfn "%i" (elapsedTimeInMinutes 3 20)

    // open Quadratic
    // printfn "%s" (calculate 1 3 1) // Two: -0.381966 and -2.618034
    // printfn "%s" (calculate 1 2 1) // One: -1.000000
    // printfn "%s" (calculate 1 2 3) // None

    // open Dice
    // let maybeDie = Die.tryCreate 20
    // let die =
    //     maybeDie
    //     |> Option.defaultValue Die.standard
    // rollPrintOnce die
    // rollPrintMultiple 5 die

    // printfn "%d" (Concepts.FunctionComposition.add5Times3 3)
    // printfn "%d" (Concepts.FunctionComposition.add1Twice 3)
    // printfn "%d" (Concepts.FunctionComposition.add1ThenMultiply 3 10)
    // printfn "%d" (Concepts.FunctionComposition.times10Add1 5)

    // printfn "%A" (Collections.Fold.foldIt 0)
    // printfn "%A" (Collections.Unfold.squareUpTo 1 101)

    // open Operators
    // printfn "%i" (PipeForward.composedF 16)
    // printfn "%i" (PipeForward.composedF2 16)
    // printfn "%i" (PipeForward.pipelined 16)
    // printfn "%A" (DoublePipeForward.composedF1 (1, 5))
    // printfn "%A" (DoublePipeForward.composedF2 (1, 5))
    // printfn "%A" (DoublePipeForward.pipelined (1, 5))

    // printfn "%A" Functions.Folds.folded
    // printfn "%A" Functions.Folds.foldedBack
    // printfn "%A" Functions.Folds.evenNumbers
    // printfn "%A" Functions.Reduce.reduced
    // printfn "%A" Functions.Reduce.reducedBack

    // printfn "%A" Collections.Functions.cheapestJaItemsCount
    // printfn "%A" Collections.Functions.jaItems
    // printfn "%A" Collections.Functions.usItems
    // printfn "%s" Collections.Functions.summary

    // Recursion.Practice.printEachChar "日本鹿児島県"

    open ComputationExpressions.Practice1b

    printfn "%A" (printResult good)
    printfn "%A" (printResult bad)
