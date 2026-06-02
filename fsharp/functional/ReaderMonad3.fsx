#r "nuget: CCFSharpUtils"
#r "nuget: FSharpPlus"
#r "nuget: FsToolkit.ErrorHandling"
open CCFSharpUtils
open CCFSharpUtils.Collections
open CCFSharpUtils.Operators
open CCFSharpUtils.Text
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Operators
open FsToolkit.ErrorHandling
open FsToolkit.ErrorHandling.Operator
open FsToolkit.ErrorHandling.Operator.Result

open FSharpPlus

type Settings = { ApiKey: string; Timeout: int; Debug: bool }

// Reader<Settings, 'a> type alias for clarity
type Reader<'a> = Reader<Settings, 'a>

// Helper to lift pure values
let ask = Reader.ask//<Settings>

// Functions that depend on settings
let getApiKey: Reader<string> =
    Reader.map (fun s -> s.ApiKey) ask

let getTimeout: Reader<int> =
    Reader.map (fun s -> s.Timeout) ask

let callApi (endpoint: string): Reader<string> =
    monad {
        let! key = getApiKey
        let! timeout = getTimeout
        return sprintf "Calling %s with key %s (timeout: %d)" endpoint key timeout
    }

let logMessage (msg: string): Reader<unit> =
    monad {
        let! settings = ask
        if settings.Debug then
            printfn "[DEBUG] %s" msg
        return ()
    }

let workflow: Reader<string> =
    monad {
        do! logMessage "Starting workflow"
        let! result = callApi "/users"
        do! logMessage (sprintf "Got result: %s" result)
        return result
    }

// Run it
let settings = { ApiKey = "secret123"; Timeout = 5000; Debug = true }
let finalResult = Reader.run workflow settings
printfn "%s" finalResult
