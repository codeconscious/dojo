open System
open Library

module Exercise01 =
    printfn("Hello, World!")

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

    open Dice

    let sixSided = createDie 20
    rollPrintOnce sixSided

    let twentySided = createDie 20
    rollPrintMultiple 3 twentySided
