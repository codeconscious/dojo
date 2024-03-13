module ActivePatterns

// Source: https://jason-down.com/2017/01/24/f-pattern-matching-part-2-active-patterns/
module SingleCaseExample1 =
    // This won't work as a pattern match case because the "pattern discriminator
    // ‘countWords’ is not defined" (as the error will display):
    // let countWords (document : string) =
    //     document.Split([|' '|]).Length

    // We need a single-case active pattern, like below, to resolve it:
    let (|CountWords|) (document : string) = // This is an "active recognizer"
        document.Split([|' '|]).Length

    let documentType document =
        match document with
        | CountWords 5 -> printfn "exactly 5 words"
        | CountWords wc when wc < 20 -> printfn "tweet with %i words" wc
        | CountWords wc when wc < 200 -> printfn "email with %i words" wc
        | CountWords wc when wc < 3000 -> printfn "blog post with %i words" wc
        | CountWords wc -> printfn "novel with %i words" wc

module SingleCasePractice1 =
    open System.Text.RegularExpressions;

    type EmailAddress = EmailAddress of string
    type EmailAddressValidationResult = Invalid | Valid of int

    let (|ValidateAddress|) emailAddress =
        let rgx = @"^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$"
        let (EmailAddress emailAddress) = emailAddress

        match Regex.IsMatch(emailAddress, rgx) with
        | true -> Valid(emailAddress.Length)
        | false -> Invalid

    // Using `function` enables eliding the `match` line!
    let summarize emailAddress = function
        | ValidateAddress (Valid a) when a < 15 -> $"short ({a} chars)"
        | ValidateAddress (Valid a) when a < 25 -> $"medium ({a} chars)"
        | ValidateAddress (Valid _) -> "quite long!"
        | ValidateAddress Invalid -> "invalid"

    // Alternate form that uses the active recognizer directly, which leads to
    // simpler match cases!
    let summarize2 (ValidateAddress emailAddress) = function
        | (Valid a) when a < 15 -> $"short ({a} chars)"
        | (Valid a) when a < 25 -> $"medium ({a} chars)"
        | (Valid _) -> "quite long!"
        | Invalid -> "invalid"

    // You can use the active recognizer directly??
    let (ValidateAddress length) as goodAddress = EmailAddress "x"

    // Not doable?
    // let (ValidateAddress _) as badAddress = EmailAddress "x"

// Source: https://jason-down.com/2017/01/24/f-pattern-matching-part-2-active-patterns/
module ParameterizedSingleCaseExample1 =
    open System

    // Key point: The value from the `match` (in this case, the text identifier) expression
    // is passed in as the last value to the active recognizer.
    let (|GreaterThanThreshold|) (word:string) threshold (text:string) =
        let occurrences =
            text.Split([|' '|])
            |> Array.filter (fun w -> w.IndexOf(word, StringComparison.InvariantCultureIgnoreCase) >= 0)

        occurrences.Length > threshold

    // Version 1:
    // let profanityChecker text =
    //     match text with
    // Version 2:
    let profanityChecker = function
        | GreaterThanThreshold "badWord1" 2 true -> // The `text` is inserted after the `2`?
            printfn "You said badWord1 more than twice. That's not acceptable!"
        | GreaterThanThreshold "badWord2" 0 true ->
            printfn "You can't say badWord2 ever!"
        | _ -> printfn "Profanity levels acceptable."

    // Profanity levels acceptable.
    "La la la badword1. I like to say badWord1 because I can." |> profanityChecker

    // You said badWord1 more than twice. That's not acceptable!
    "Don't give me that badWord1 talk you badWord1. Or else badWord1." |> profanityChecker

    // You can't say badWord2 ever!
    "I'm a badWord2 person myself." |> profanityChecker

module MultipleCaseExample1 =
    let (|Even|Odd|) number =
        if number % 2 = 0 then Even else Odd

    let describeNumber = function
        | Even -> printfn "The number is even."
        | Odd -> printfn "The number is odd."

    describeNumber 42 // The number is even.
    describeNumber 1337 // The number is odd.

module MultipleCaseExample2 =
    open System

    let (|Spring|Summer|Fall|Winter|) (date:DateTime) =
        match date.Month, date.Day with
        | 1, _ | 2, _ -> Winter
        | 3, d when d < 21 -> Winter
        | 3, _ | 4, _ | 5, _ -> Spring
        | 6, d when d < 21 -> Spring
        | 6, _ | 7, _ | 8, _ -> Summer
        | 9, d when d < 21 -> Summer
        | 9, _ | 10, _ | 11, _ -> Fall
        | 12, d when d < 21 -> Fall
        | _ -> Winter

    let printCanadianSeason = function
        | Spring | Summer | Fall -> printfn "Construction season"
        | Winter -> printfn "Winter season"

    let january = new DateTime(2017, 01, 20)

    january |> printCanadianSeason // Winter season
    6 |> january.AddMonths |> printCanadianSeason // Construction season

module PartialActivePatternExample1 =
    // The underscore signals that we are dealing with a partial active pattern,
    // which then requires us to return an option type. If this was not used,
    // `true` would have to be specified in the `match` cases below (e.g., `Fizz true`).
    let (|Fizz|_|) n = if n % 3 = 0 then Some Fizz else None
    let (|Buzz|_|) n = if n % 5 = 0 then Some Buzz else None

    let fizzBuzz = function
        | Fizz & Buzz -> "FizzBuzz"
        | Fizz -> "Fizz"
        | Buzz -> "Buzz"
        | n -> sprintf "%i" n

    [1..100] |> List.iter (fizzBuzz >> printfn "%s")

module ParameterizedPartialActivePatternExample1 =
    let (|MultipleOf|_|) (multiplicand : int) (number : int) =
        if (number % multiplicand = 0) then Some MultipleOf else None

    let (|Fizz|_|) = (|MultipleOf|_|) 3
    let (|Buzz|_|) = (|MultipleOf|_|) 5

    let fizzBuzz = function
        | Fizz & Buzz -> "FizzBuzz"
        | Fizz -> "Fizz"
        | Buzz -> "Buzz"
        | n -> sprintf "%i" n

    [1..100] |> List.iter (fizzBuzz >> printfn "%s")

// module ParameterizedSingleCaseExercise1 =
//     type JPY = JPY of uint
//     type USD = USD of decimal
//     type Currency = JPY | USD

//     let (|CharacterTypeChecker|)

(*
module Sample =
    let inputList = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]

    let status =
        {| SourceList = inputList
            SourceCount = 0
            SourceWeight = 0
            TargetList = list<int>.Empty
            TargetCount = 0
            TargetWeight = 0 |}

    let afterWork =
        status
        |> (fun st ->
            {| st with
                SourceCount = st.SourceList.Length |})
        |> (fun st ->
            {| st with
                SourceWeight = List.sum st.SourceList |})
        |> (fun st ->
            {| st with
                TargetList = List.filter (fun x -> x % 2 = 0) st.SourceList |})
        |> (fun st ->
            {| st with
                TargetCount = st.TargetList.Length |})
        |> (fun st ->
            {| st with
                TargetWeight = List.sum st.TargetList |})

    printf
        "Job Complete. Source Count: %d, Source Total: %d, Target Count: %d, Target Total: %d"
        afterWork.SourceCount
        afterWork.SourceWeight
        afterWork.TargetCount
        afterWork.TargetWeight
*)
