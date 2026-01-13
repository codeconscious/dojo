// Source: https://atcoder.jp/contests/abc391/tasks/abc391_a

let opposites dir =
    let directions = ['N'; 'E'; 'S'; 'W']
    let opposites = directions |> List.permute (fun x -> (x + 2) % 4)
    List.zip directions opposites
    |> Map.ofList
    |> Map.find dir

let run (dir: string) =
    let validInputs = ["N"; "E"; "W"; "S"; "NE"; "NW"; "SE"; "SW"]
    if validInputs |> List.contains dir then
        dir.ToCharArray()
        |> Array.map opposites
        |> System.String
    else
        "ERROR: Invalid input!"

let runAuto =
    ["N"; "E"; "W"; "S"; "NE"; "NW"; "SE"; "SW"]
    |> List.randomChoice
    |> _.ToCharArray()
    |> Array.map opposites
    |> System.String
