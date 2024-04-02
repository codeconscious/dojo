open System
open System.IO
open System.Text

(* TODOs:
   -
*)

let rawArgs =
    fsi.CommandLineArgs
    |> Array.toList
    |> List.tail

let quoteText = "> "

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
    match extractArgs rawArgs with
    | Ok (limit, files) ->
        if limit <= quoteText.Length
        then invalidOp "The line length limit cannot exceed the quotation text length."
        else limit - quoteText.Length - 1, files
    | Error e -> invalidOp e

let read path =
    try
        path
        |> File.ReadAllText
        |> Ok
    with
        | :? FileNotFoundException -> Error $"\"{path}\" was not found."
        | e -> Error $"Settings unexpectedly could not be read from \"{path}\": {e.Message}"

let rec splitLine limit line =
    let lastSpaceIndex (str:string) (searchCeilingIndex:int) =
        let endIndex = str.Length - 1
        let resultIndex = str.LastIndexOf(" ", searchCeilingIndex)
        match resultIndex with
        | -1 -> endIndex
        | _ -> resultIndex
    let rec loop acc limit (lineInner:string) =
        match lineInner.Length with
        | l when l <= limit -> acc @ [lineInner]
        | _ ->
            let index = lastSpaceIndex lineInner limit
            let head = lineInner[..index]
            let tail = lineInner[index+1..]
            loop (acc @ [head]) limit tail
    loop [] limit line

let enquoten quoteText line =
    sprintf "%s%s" quoteText line

files
|> Seq.map (fun f -> read f)
|> Seq.iter (fun l0 -> match l0 with
                       | Ok l -> l.Split Environment.NewLine
                                 |> Array.toList
                                 |> List.map
                                     (fun l -> splitLine limit l)
                                 |> List.collect (fun l -> l)
                                 |> List.map (fun l -> enquoten quoteText l)
                                 |> List.iter (fun l -> printfn "%s" l)
                       | Error e -> printfn "%s" e)
