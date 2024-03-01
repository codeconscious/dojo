let args = fsi.CommandLineArgs |> Array.tail // The head is the filename
match args with
| a when a.Length = 0 -> "none"
| a -> String.concat "\" and \"" a
|> printfn "Argument(s): \"%s\""
