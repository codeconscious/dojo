open System
open System.IO

(* TODOs:
   - Avoid exceptions where possible
   - Custom quote prefix
*)

let args =
    fsi.CommandLineArgs
    |> Array.toList
    |> List.tail // The head contains the script filename.

let prefix = "> "

let extractArgs (args:string list) =
    match args with
    | width::files -> match width |> System.Int32.TryParse with
                      | true, int ->
                            match int with
                            | i when i > 0 -> Ok(int, files |> List.distinct)
                            | _ -> Error (sprintf "The width \"%s\" is invalid. It must be greater than 0." width)
                      | _ -> Error (sprintf "The width \"%s\" is invalid. It must be greater than 0." width)
    | _ -> Error <| sprintf "Invalid args provided."

let (limit, files) =
    match extractArgs args with
    | Ok (limit, files) ->
        if limit <= prefix.Length
        then invalidOp "The line length limit cannot exceed the quotation text length."
        else limit - prefix.Length - 1, files
    | Error e -> invalidOp e

let read path =
    try
        path
        |> File.ReadAllText
        |> Ok
    with
        | :? FileNotFoundException -> Error $"\"{path}\" was not found."
        | e -> Error $"Settings unexpectedly could not be read from \"{path}\": {e.Message}"

let rec splitLine lengthLimit fullLine =
    let finalSpaceIndex (text:string) (indexCeiling:int) =
        let resultIndex = text.LastIndexOf(" ", indexCeiling) // Searches backwards
        match resultIndex with
        | -1 -> text.Length - 1
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

let enquoten prefix line =
    sprintf "%s%s" prefix line

files
|> Seq.map (fun f -> read f)
|> Seq.iter (fun l ->
    match l with
    | Ok l ->
        l.Split Environment.NewLine
        |> Array.toList
        |> List.map (fun l -> splitLine limit l)
        |> List.collect (fun l -> l)
        |> List.map (fun l -> enquoten prefix l)
        |> List.iter (fun l -> l |> printfn "%s")
    | Error e -> printfn "%s" e)
