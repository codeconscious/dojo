(* ENQUOTEN F# Script

   Summary: Reads one or more text-only files and splits each file's lines to the
            maximum line length provided. Each line is prefixed with "> ", and the
            prefix is included in the line length calculation.

   Requirements: .NET 8 SDK

   Usage: dotnet fsi <lineLengthLimit> <filePath(s)>
          Sample: `dotnet fsi 70 'Documents/file1.txt'
          Sample: `dotnet fsi 50 'Documents/file1.txt' 'Documents/file2.log'

   TODOs and improvement ideas:
   - Incorporate computation expressions!
   - Validation: Check for no files submitted
   - Validation: Filter out files with errors (e.g., missing files) first
   - Avoid exceptions where possible, perhaps using computation expressions
   - Allow custom quote prefixes (maybe)
*)

open System
open System.IO

let args =
    fsi.CommandLineArgs
    |> Array.toList
    |> List.tail // The head contains the script filename.

let prefix = "> "

let extractArgs (args:string list) =
    match args with
    | lengthLimit::files ->
        match lengthLimit |> System.Int32.TryParse with
        | true, int ->
            match int with
            | i when i > prefix.Length + 1 -> Ok(int, files |> List.distinct)
            | _ -> Error <| sprintf "The width \"%s\" is too low. It must be greater than 0." lengthLimit
        | _ -> Error <| sprintf "The width \"%s\" is invalid. Provide a number greater than 0." lengthLimit
    | _ -> Error <| sprintf "Invalid args provided. Enter a maximum length and one or more file paths."

let (limit, files) =
    match extractArgs args with
    | Ok (limit, files) ->
        if limit <= prefix.Length
        then invalidOp "The line length limit cannot exceed the quotation text length."
        else limit - prefix.Length - 1, files
    | Error e -> invalidOp e

let readFile path =
    try
        path
        |> File.ReadAllText
        |> Ok
    with
        | :? FileNotFoundException -> Error $"\"{path}\" was not found."
        | e -> Error $"Settings unexpectedly could not be read from \"{path}\": {e.Message}"

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
            let head = linePart[..splitIndex]
            let tail = linePart[splitIndex+1..]
            loop (acc @ [head]) tail limit
    loop [] fullLine lengthLimit

let enquoten prefix text =
    sprintf "%s%s" prefix text

files
|> Seq.map (fun f -> readFile f)
|> Seq.iter (fun l ->
    match l with
    | Ok l ->
        l.Split Environment.NewLine
        |> Array.toList
        |> List.map (fun l -> splitLine l limit)
        |> List.collect (fun l -> l)
        |> List.map (fun l -> enquoten prefix l)
        |> List.iter (fun l -> l |> printfn "%s")
    | Error e -> printfn "%s" e)
