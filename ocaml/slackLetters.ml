module Styles = struct
    let letters = List.init 26 (fun i -> Char.chr (Char.code 'a' + i)) (* [ 'a' .. 'z' ] *)
    let numbers = List.init 10 (fun i -> Char.chr (Char.code '0' + i))
    let space = ' '

    type style = {
        name: string;
        supported_chars: char list;
        converter: char -> string
    }

    let styles =
        [
            { name = "cookie";
              supported_chars = letters @ [space; '!'; '?'; '&'; (* '•' *)];
              converter =
                    fun ch ->
                        match ch with
                        | c when c = space -> ":blank:"
                        | '!' -> ":cookie-exclaim:"
                        | '?' -> ":cookie-question:"
                        | '&' -> ":cookie-and:"
                        (* | '•' -> ":cookie-dot:" *)
                        | c -> Printf.sprintf ":cookie-{%c}:" c
            };
            { name = "bluebox";
              supported_chars = letters @ [space];
              converter =
                    fun ch ->
                        match ch with
                        | c when c = space -> ":blank: "
                        | 'o' -> ":alpha-0: "
                        | c -> Printf.sprintf ":alpha-{%c}:" c (* Hairspace *)
            };
            { name = "alphawhite";
              supported_chars = letters @ [space; '!'; '?'; '#'; '@'];
              converter =
                    fun ch ->
                        match ch with
                        | c when c = space -> ":blank:"
                        | '!' -> ":alphabet-white-exclamation:"
                        | '?' -> ":alphabet-white-question:"
                        | '#' -> ":alphabet-white-hash:"
                        | '@' -> ":alphabet-white-at:"
                        | c -> Printf.sprintf ":alphabet-white-{%c}:" c
            };
            { name = "alphayellow";
              supported_chars = letters @ [space; '!'; '?'; '#'; '@'; (* 'ñ' *)];
              converter =
                    fun ch ->
                        match ch with
                        | c when c = space -> ":blank:"
                        | '!' -> ":alphabet-yellow-exclamation:"
                        | '?' -> ":alphabet-yellow-question:"
                        | '#' -> ":alphabet-yellow-hash:"
                        | '@' -> ":alphabet-yellow-at:"
                        (* | 'ñ' -> ":alphabet-yellow-nñ:" *)
                        | c -> Printf.sprintf ":alphabet-yellow-{%c}:" c
            };
            { name = "alphasnow";
              supported_chars = letters @ [space];
              converter =
                    fun ch ->
                        match ch with
                        | c when c = space -> ":blank:"
                        | 'i' -> ":alpha_snow_i2:"
                        | c -> Printf.sprintf ":alpha_snow_{%c}:" c
            };
            { name = "tiles";
              supported_chars = letters @ [space; (* 'ñ' *)];
              converter =
                    fun ch ->
                        match ch with
                        | c when c = space -> ":letter_blank:"
                        (* | 'ñ' -> ":letterñ:" *)
                        | c -> Printf.sprintf ":letter_{%c}:" c
            };
            { name = "pokemon";
              supported_chars = letters @ [space];
              converter =
                    fun ch ->
                        match ch with
                        | c when c = space -> ":blank:"
                        | c -> Printf.sprintf ":pokemonfont-{%c}:" c
            };
            { name = "merah";
              supported_chars = letters @ [space];
              converter =
                    fun ch ->
                        match ch with
                        | c when c = space -> ":blank:"
                        (* The "merah" set is incomplete and inconsistently named. *)
                        | 'b' -> ":merahbb:"
                        | 'd' -> ":merahdd:"
                        | 'e' -> ":merahee:"
                        | 'f' -> ":ff:"
                        | 'h' -> ":merahhh:"
                        | 'k' -> ":merahkk:"
                        | 'l' -> ":merahll:"
                        | 'q' -> ":alpha-q:"
                        | 'r' -> ":merahrr:"
                        | 's' -> ":merahsss:"
                        | 'u' -> ":merahuuu:"
                        | 'x' -> ":alpha-x:"
                        | 'y' -> ":merahhyyy:"
                        | 'z' -> ":alpha-z:"
                        | c -> Printf.sprintf ":merah{%c}:" c
            };
            { name = "magazine";
              supported_chars = letters @ [space];
              converter =
                    fun ch ->
                        match ch with
                        | c when c = space -> ":blank:"
                        | c -> Printf.sprintf ":magazine_{%c}:" c
            };
            { name = "custom";
              supported_chars = letters @ numbers @ [space; '?'; '!'];
              converter =
                    fun ch ->
                        match ch with
                        | c when c = space -> ":blank:"
                        | 'a' -> ":m-a:"
                        | 'b' -> ":b4:"
                        | 'c' -> ":c2:"
                        | 'd' -> ":devo-d:"
                        | 'e' -> ":edge:"
                        | 'f' -> ":ff:"
                        | 'g' -> ":g-neon:"
                        | 'h' -> ":h:"
                        | 'i' -> ":info:"
                        | 'j' -> ":super-j:"
                        | 'k' -> ":m'kay:"
                        | 'l' -> ":labsslide-1:"
                        | 'm' -> ":m:"
                        | 'n' -> ":n64:"
                        | 'o' -> ":o:"
                        | 'p' -> ":p2:"
                        | 'q' -> ":qflash:"
                        | 'r' -> ":r:"
                        | 's' -> ":scon:"
                        | 't' -> ":kid-t:"
                        | 'u' -> ":m-u:"
                        | 'v' -> ":devo-v:"
                        | 'w' -> ":walphabet:"
                        | 'x' -> ":x:"
                        | 'y' -> ":y1:"
                        | 'z' -> ":zelle_onfire:"
                        | '0' -> ":0_bats:"
                        | '1' -> ":number-1-red:"
                        | '2' -> ":number2:"
                        | '3' -> ":number-3-flip:"
                        | '4' -> ":mana-4:"
                        | '5' -> ":round-red-5:"
                        | '6' -> ":mana-6:"
                        | '7' -> ":7-up:"
                        | '8' -> ":8flower:"
                        | '9' -> ":9lego:"
                        | '!' -> ":exclamation:"
                        | '?' -> ":question-icon:"
                        | _ -> String.make 1 ch (* Should not be reached. *)
            };
            { name = "numbers";
              supported_chars = numbers @ [space];
              converter =
                    fun ch ->
                        match ch with
                        | c when c = space -> ":blank:"
                        | c -> Printf.sprintf ":num{%c}:" c
            };
            { name = "squarenumbers";
              supported_chars = numbers @ [space];
              converter =
                    fun ch ->
                        match ch with
                        | c when c = space -> ":blank:"
                        | '0' -> ":zero:"
                        | '1' -> ":one:"
                        | '2' -> ":two:"
                        | '3' -> ":three:"
                        | '4' -> ":four:"
                        | '5' -> ":five:"
                        | '6' -> ":six:"
                        | '7' -> ":seven:"
                        | '8' -> ":eight:"
                        | _ -> "nine"
            };
        ]

    let styleNames = styles |> List.map (fun s -> s.name)

end

module ArgValidation = struct
    open Styles

    type user_args = { style: string; text: string }

    let validate =
        let supportedStyleNames =
            String.Join(
                space,
                styles |> List.map _.Name
            )

        let rawArgs =
            fsi.CommandLineArgs
            |> Array.toList
            |> List.tail // The head contains the script filename.

        let argCount (rawArgs:string list) =
            let errorText =
                String.Join(
                    Environment.NewLine,
                    ["Pass in (1) a style name and (2) a string containing only supported characters for that style.";
                    $"Supported styles: {supportedStyleNames}"]
                )

            match rawArgs.Length with
            | l when l = 2 ->
                Ok <|
                {
                    Style = rawArgs.Head.ToLowerInvariant();
                    Text = rawArgs[1].ToLowerInvariant()
                }
            | _ -> Error errorText

        let styleName args =
            let errorText =
                String.Join(
                    Environment.NewLine,
                    [$"Style \"{args.Style}\" not found.";
                    $"Supported styles: {supportedStyleNames}"]
                )

            match styleNames |> List.contains args.Style with
            | true -> Ok args
            | false -> Error errorText

        let inputLength args =
            match args.Text.Length with
            | 0 -> Error "You must enter text to be converted."
            | _ -> Ok args

        let inputChars args =
            let style =
                styles
                |> List.filter (fun s -> s.name = args.Style);
                |> List.head

            let ensureVisibleChar ch =
                if ch = space then "<SPACE>" else $"{%c}" c

            let invalidChars =
                args.Text
                |> Seq.toList
                |> List.filter (fun ch -> not <| List.contains ch style.SupportedChars)
                |> List.map ensureVisibleChar

            let error style =
                let supported_chars style =
                    let chars =
                        style.SupportedChars
                        |> List.map ensureVisibleChar
                    String.Join(space, chars)

                String.Join(
                    Environment.NewLine,
                    [$"Invalid characters found for style \"{style.Name}\": {String.Join(space, invalidChars)}";
                    $"Supported characters: {style |> supported_chars}"]
                )

            if invalidChars.Length = 0
            then Ok args
            else Error <| error style

        result {
            let! args = argCount rawArgs
            let! args' = styleName args
            let! args'' = inputLength args'
            let! args''' = inputChars args''
            return args'''
        }

open ArgValidation
open Styles

let args = validate

let getStyle args =
    styles
    |> List.filter (fun s -> s.name = args.Style);
    |> List.head

let convertText args style =
    let text = args.Text
    let converter = style.Converter

    text
    |> Seq.map converter
    |> Seq.map string
    |> String.concat System.String.Empty

match args with
| Error e ->
    e |> eprintfn "%s"
| Ok a ->
    a
    |> getStyle
    |> convertText a
    |> printfn "%s"
