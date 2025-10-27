(* Selected JavaScript problems in F#. *)

#r "nuget:  FSharpPlus"

open System
open System.Text
open FSharpPlus.Operators

let ensureEqual expected actual  =
    if actual <> expected
    then printfn $"NOT EQUAL! Expected: {expected} / Actual: {actual}"

(* https://github.com/0x66796e6e/interview-preparation/blob/master/markdown/easy/easy-js-questions.md *)
module Easy =
    module One = // Convert an array of strings to an array of the lengths of those strings
        let input = [ "this"; "is"; "an"; "array" ]
        let expected = [ 4; 2; 2; 5 ]

        let run () =
            input
            |> map _.Length
            |> ensureEqual expected

    module Two = // Sum an array of numbers
        let input = [1; 2; 3; 4; 5]
        let expected = 15

        let run () = input |> List.sum |> ensureEqual expected

    module Three = // Write a function that can be called like this: greeter("Hello")("Candidate");
        let expected = "Hello, Candidate!"
        let f hello = fun candidateName -> $"%s{hello}, %s{candidateName}!"

        let run () = f "Hello" "Candidate" |> ensureEqual expected // Same as `f("Hello")("Candidate")`

    module Five = // Write a function that returns whether a string is a palindrome. Less than 160 characters is preferred.
        let input = "abccba"
        let expected = true

        let run () =
            input.ToCharArray()
            |> rev
            |> String
            |> (=) input
            |> ensureEqual expected

    module Nine = // Anagram checker
        let input = ["fynn"; "nyfn"]
        let expected = true

        let run () =
            input
            |> List.map (fun x ->
                x.ToCharArray()
                |> Array.countBy id
                |> sort)
            |> Set.ofList
            |> _.Count
            |> (=) 1
            |> ensureEqual expected

        let run' () =
            input
            |> map (fun x -> x.ToCharArray() |> sort)
            |> Set.ofList
            |> _.Count
            |> (=) 1
            |> ensureEqual expected

        let run'' () =
            input
            |> map (fun word -> word.ToCharArray() |> sort )
            |> distinct
            |> _.Length
            |> (=) 1

    module Ten = // Count vowels
        let input = "Aloha! My name is Fynn."
        let expected = 6

        let run () =
            let vowels = [| 'a'; 'e'; 'i'; 'o'; 'u' |]
            input.ToCharArray()
            |> Array.where (fun x -> Array.contains (Char.ToLowerInvariant x) vowels)
            |> _.Length
            |> ensureEqual expected

    module Twelve = // Return the most common word in a given string.
        let input = "hello my name is fynn and this is kind of funny. Is this real?"
        let expected = "is"

        let run () =
            input.Split(' ')
            |> Array.countBy id
            |> sortByDescending snd
            |> head
            |> fst
            |> ensureEqual expected

    module Fifteen = // Create a function which multiplies all given parameters.
        let input = [2; 3; 4]
        let expected = 24

        let run () = input |> List.reduce (*) |> ensureEqual expected

    module Sixteen = // Return the longest word in a given string.
        let input = "Hello my name is Fynn!!"
        let expected = "Hello"

        let run () =
            let letters = Array.concat [ [| 'a'..'z' |]; [| 'A'..'Z' |] ]
            let isLetter ch = Array.contains ch letters

            let stripInvalidChars (word: string) =
                word.ToCharArray()
                |> filter isLetter
                |> String

            input.Split(' ')
            |> map stripInvalidChars
            |> Array.sortByDescending _.Length
            |> head
            |> ensureEqual expected

        let run' () =
            let validChars = List.concat [ [ 'a'..'z' ]; [ 'A'..'Z' ]; [' '] ]
            let isValidChar ch = List.contains ch validChars

            input.ToCharArray()
            |> filter isValidChar
            |> String
            |> fun x -> x.Split(' ')
            |> Array.sortByDescending _.Length
            |> head
            |> ensureEqual expected

    module Seventeen = // Write a function that accepts a number N as argument and adds all values from N to 1.
        let input = 4
        let expected = 10

        let run () =
            [1..input]
            |> List.reduce (+)
            |> ensureEqual expected

    module Eighteen = // Remove duplicate characters from a string.
        let input = "Hello my name is Fynn"
        let expected = "helo mynaisf"

        let run () =
            input.ToLowerInvariant().ToCharArray()
            |> distinct
            |> String
            |> ensureEqual expected

    module TwentyTwo = // Write a factorial function:
        let input = [4..-1..1]
        let expected = 24

        let run () =
            input
            |> List.reduce (*)
            |> ensureEqual expected

Easy.One.run()
Easy.Two.run()
Easy.Three.run()
Easy.Five.run()
Easy.Nine.run()
Easy.Nine.run'()
Easy.Nine.run''()
Easy.Ten.run()
Easy.Twelve.run()
Easy.Fifteen.run()
Easy.Sixteen.run()
Easy.Sixteen.run'()
Easy.Seventeen.run()
Easy.Eighteen.run()
Easy.TwentyTwo.run()

module Medium =
    module Three = // Reverse each word in a string.
        let input = "Hello my name is Fynn"
        let expected = "olleH ym eman si nnyF"

        let run () =
            input.Split ' '
            |> map (fun word -> word.ToCharArray() |> rev |> String)
            |> String.concat " "
            |> ensureEqual expected

    module Eight = // Find the maximum sum of products in two arrays.
        let input = [ [3; 4; 1; 2]; [9; 4; 8; 2] ]
        let expected = 70 // Abbreviated output, eliding the string.

        let run () =
            input
            |> map sort
            |> transpose
            |> map (List.reduce (*))
            |> sum
            |> ensureEqual expected

    module Eleven = // Convert a string to "Jaden Case" (TIL: Named after Will Smith's son...)
        let input = "Hello my name is Fynn"
        let expected = "Hello My Name Is Fynn"

        let run () =
            input.Split " "
            |> Array.map (fun word -> $"{Char.ToUpperInvariant word[0]}{word[1..]}")
            |> String.concat " "
            |> ensureEqual expected

    module Seventeen = // Check if the parenthesis are balanced.
        let input = "(({}){})"
        let expected = true

        let run () =
            let rec check (text: string) =
                text.Replace("{}", String.Empty)
                    .Replace("()", String.Empty)
                |> function
                | "" -> true
                | x when x.Length = text.Length -> false
                | x -> check x

            check input |> ensureEqual expected

        let run' () = // Uses a StringBuilder for greater efficiency.
            let rec check attemptsRemaining (workingText: StringBuilder) =
                workingText
                    .Replace("{}", String.Empty)
                    .Replace("()", String.Empty)
                |> function
                | x when x.Length = 0 -> true
                | x ->
                    if attemptsRemaining = 0
                    then false
                    else check (attemptsRemaining - 1) x

            input |> StringBuilder |> check 10 |> ensureEqual expected

Medium.Three.run()
Medium.Eight.run()
Medium.Eight.run()
Medium.Eleven.run()
Medium.Seventeen.run()
Medium.Seventeen.run'()
