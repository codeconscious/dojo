let say_hi () = print_endline "Hello world!"

type person = { first_name : string}

let people = [
    { first_name = "JJ" };
    { first_name = "KK" };
    { first_name = "LL" }
]

let () =
    people
    |> List.map (fun x -> String.lowercase_ascii x.first_name)
    |> List.sort String.compare
    |> List.rev
    |> String.concat ","
    |> Printf.printf "%s"
