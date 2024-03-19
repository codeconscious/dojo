// In full projects, perhaps `System.Environment.GetCommandLineArgs() is equivalent.
let args = fsi.CommandLineArgs |> Array.tail // The head is the filename

match args with
| a when a.Length = 0 -> "none"
| a -> String.concat "; " a
|> printfn "Argument(s): %s"
