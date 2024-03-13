module Options

type BillingDetails = {
        Name : string
        BillingAddress : string
        DeliveryAddress : string option // if `option` is missing, the func below breaks
    }

module DefaultValue =
    let addressForPackage (details : BillingDetails) =
        let address =
            details.DeliveryAddress
            |> Option.defaultValue details.BillingAddress
        sprintf "%s\n%s" details.Name address

(* `bind` vs. `map`:
    • `map  f inp` evaluates to `match inp with None -> None | Some x -> Some (f x)`
    • `bind f inp` evaluates to `match inp with None -> None | Some x -> f x`
*)
module Map =
    // Transforms an option value by using a specified mapping function. ('T -> 'U)
    let printDeliveryAddress (details : BillingDetails) =
        details.DeliveryAddress
        |> Option.map (fun addr -> addr.ToUpperInvariant())
        |> Option.iter (fun addr -> printfn "Address:\n%s\n%s" (details.Name.ToUpper()) addr)

module Bind =
    // Invokes a function on an optional value that *itself yields an option*. ('T -> 'U option)
    // Useful because you can pipe together multiple functions because they will all receive and return options.
    open System

    let tryLastLine (address : string) =
        let parts = address.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
        parts |> Array.tryLast
        // Alternate version:
        // match parts with
        // | [||] -> None
        // | parts -> parts |> Array.last |> Some

    let tryPostalCode (codeString : string) =
        match Int32.TryParse(codeString) with
        | true, i -> i |> Some
        | false, _ -> None

    let postalCodeHub (code : int) =
        if code = 77777 then "Hub 1" else "Hub 2"

    let tryHub (details : BillingDetails) =
        details.DeliveryAddress
        |> Option.bind tryLastLine
        |> Option.bind tryPostalCode
        |> Option.map postalCodeHub

module Nulls =
    open System
    module ToOption =
        let myApiFn (str : string) =
            let s = // guaranteed not to be null!
                str
                |> Option.ofObj
                |> Option.defaultValue "(none)"
            printfn "%s" (s.ToUpper())

        let showHeartRate (rate : Nullable<int>) =
            rate
            |> Option.ofNullable
            |> Option.map (fun r -> r.ToString())
            |> Option.defaultValue "N/A"
        showHeartRate (Nullable(24)) |> ignore

    module ToNullable =
        let tryLocationDescription (locationId : int) =
            let random = new Random()
            let r = random.Next(1, 100)
            if r < 50 then
                Some (sprintf "Location number %i" r)
            else
                None
        let tryLocationDescriptionNullable (locationId : int) =
            tryLocationDescription(locationId)
            |> Option.toObj

        // Also, `.toNullable`

module ValueOptions =
    (* Value types that live on the stack. Can be good for performance, but
       consider the cost of copying when using them. *)
    let valueOptionString (v : int voption) =
        match v with
        | ValueSome x -> sprintf "Value: %i" x
        | ValueNone -> sprintf "No value"

    ValueOption.ValueNone |> valueOptionString |> ignore // "No value"
    ValueOption.ValueSome 99 |> valueOptionString |> ignore // "Value: 99"
