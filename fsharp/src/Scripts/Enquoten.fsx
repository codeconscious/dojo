(* ENQUOTEN F# Script

   Summary: Reads a text-only file and splits its lines to the maximum line length
            limit provided. Each line is prefixed with "> ", and the prefix is included
            in the line length calculation.

   Requirements: .NET 8 SDK (Untested on previous versions, though it might work)

   Usage: dotnet fsi <lineLengthLimit> <filePath(s)>
          Sample: dotnet fsi 70 'Documents/file1.txt'

   TODOs and improvement ideas:
   - Incorporate computation expressions! (Priority)
   - Allow optionally disabling space checking for better Japanese support
   - Allow custom quote prefixes
*)

open System
open System.IO

let args =
    fsi.CommandLineArgs
    |> Array.toList
    |> List.tail // The head contains the script filename.

let quoteText = "> "

// type ResultBuilder() =
//     member this.Bind(m, f) =
//         match m with
//         | Error e -> Error e
//         | Ok a ->
//             printfn "\tSuccessful: %A" a
//             f a

//     member this.Return(x) =
//         Ok x

// let result = new ResultBuilder()

type Args =
    { Limit: int
      File: string }

let validateArgCount (args:string list) =
    match args.Length with
    | l when l = 2 -> Ok (args.Head, args.Tail.Head)
    | _ -> Error "You must supply a maximum line length and one file."

let validateLimit (args:string * string) =
    match args with
    | (l, f) ->
        match (l |> System.Int32.TryParse) with
        | true, i when i >= 10 -> Ok (i, f)
        | true, _ -> Error "Requested line length too short."
        | _ -> Error "Maximum line length must be numeric."

let validateFileExists (args:int * string) =
    match args with
    | (l, f) ->
        match f |> File.Exists with
        | true -> Ok { Limit = l; File = f }
        | false -> Error $"The file \"{f}\" does not exist."

let validateArgs (args:string list) =
    args
    |> validateArgCount
    |> Result.bind validateLimit
    |> Result.bind validateFileExists

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
        // printfn $"Length is {limit}!"
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

let validatedArgs = validateArgs args

match validatedArgs with
| Ok a ->
    readFile a.File
    |> (fun line ->
            match line with
            | Ok line ->
                line.Split Environment.NewLine
                |> Array.toList
                |> List.map (fun l -> splitLine l a.Limit)
                |> List.collect (fun l -> l)
                |> List.map (fun l -> enquoten quoteText l)
                |> List.iter (fun l -> l |> printfn "%s")
            | Error e -> printfn "%s" e)
| Error e -> printfn "Error parsing args."
