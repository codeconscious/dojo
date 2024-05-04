(* ENQUOTEN F# Script

   Summary: Reads a text-only file and splits its lines to the maximum line length
            limit provided. Each line is prefixed with "> ", and the prefix is included
            in the line length calculation.

   Requirements: .NET 8 SDK (Untested on previous versions, though it might work)

   Usage: dotnet fsi <lineLengthLimit> <filePath(s)>
          Sample: dotnet fsi 70 'Documents/file1.txt'

   Note: This was created mainly for learning purposes and might not be very good. ^_^

   Last update: May 4, 2024, to add computation expressions
*)

open System
open System.IO

let quotePrefix = "> "

type Args<'T> =
    { Limit: 'T
      File: string }

type ResultBuilder() =
    member this.Bind(m, f) =
        match m with
        | Error e -> Error e
        | Ok a -> f a

    member this.Return(x) =
        Ok x

let result = new ResultBuilder()

let validateArgs =
    let rawArgs =
        fsi.CommandLineArgs
        |> Array.toList
        |> List.tail // The head contains the script filename.

    let validateArgCount (args:string list) =
        match args.Length with
        | l when l = 2 -> Ok { Limit = args.Head; File = args.Tail.Head }
        | _ -> Error "You must supply a maximum line length and one file."

    let validateLimit (args:Args<string>) =
        match args with
        | { Args.Limit = l; Args.File = f } ->
            match (l |> System.Int32.TryParse) with
            | true, i when i >= 10 -> Ok { Limit = i; File = f }
            | true, _ -> Error "Requested line length too short."
            | _ -> Error "Maximum line length must be numeric."

    let validateFileExists (args:Args<int>) =
        match args with
        | { Args.Limit = l; Args.File = f } ->
            match f |> File.Exists with
            | true -> Ok args
            | false -> Error $"The file \"{f}\" does not exist."

    result {
        let! args = validateArgCount rawArgs
        let! args' = validateLimit args
        let! args'' = validateFileExists args'
        return args''
    }

let readFile file =
    try
        file
        |> File.ReadAllText
        |> Ok
    with
        | :? FileNotFoundException -> Error $"\"{file}\" was not found."
        | e -> Error $"Unexpectedly could not read \"{file}\": {e.Message}"

let splitLine fullLine lengthLimit =
    let finalSpaceIndex (text:string) (indexCeiling:int) =
        let resultIndex = text.LastIndexOf(" ", indexCeiling) // Searches backwards
        match text.LastIndexOf(" ", indexCeiling) with
        | -1 -> indexCeiling - 1
        | _  -> resultIndex

    let rec loop acc (linePart:string) limit =
        match linePart.Length with
        | l when l <= limit -> acc @ [linePart]
        | _ ->
            let splitIndex = finalSpaceIndex linePart limit
            let trimmed = linePart[..splitIndex]
            let remaining = linePart[splitIndex+1..]
            loop (acc @ [trimmed]) remaining limit
    loop [] fullLine lengthLimit

let enquoten prefix text =
    sprintf "%s%s" prefix text

let processFileText limit (text:string) =
    text.Split Environment.NewLine
    |> Array.toList
    |> List.map (fun l -> splitLine l limit)
    |> List.collect (fun l -> l)
    |> List.map (fun l -> enquoten quotePrefix l)

let output =
    result {
        let! validatedArgs = validateArgs
        let! fileText = readFile validatedArgs.File
        return processFileText validatedArgs.Limit fileText
    }

match output with
| Ok lines -> lines |> List.iter (fun l -> l |> printfn "%s")
| Error e -> printfn "%s" e
