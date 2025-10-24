(* Selected JavaScript problems in F#. *)

open System

let ensureEqual expected actual  =
    if actual <> expected
    then printfn $"NOT EQUAL! Expected: {expected} / Actual: {actual}"

(* https://github.com/0x66796e6e/interview-preparation/blob/master/markdown/easy/easy-js-questions.md *)
module Easy =
    module One = // Convert an array of strings to an array of the lengths of those strings
        let input = [ "this"; "is"; "an"; "array" ]
        let expected = [ 4; 2; 2; 5 ]

        let run =
            input
            |> List.map _.Length
            |> ensureEqual expected

    module Two = // Sum an array of numbers
        let input = [1; 2; 3; 4; 5]
        let expected = 15

        let run = input |> List.sum |> ensureEqual expected

    module Three = // Write a function that can be called like this: greeter("Hello")("Candidate");
        let expected = "Hello, Candidate!"
        let f hello = fun candidateName -> $"%s{hello}, %s{candidateName}!"

        let run = f "Hello" "Candidate" |> ensureEqual expected // Same as `f("Hello")("Candidate")`

    module Five = // Write a function that returns whether a string is a palindrome. Less than 160 characters is preferred.
        let input = "abccba"
        let expected = true

        let run =
            input.ToCharArray()
            |> Array.rev
            |> String
            |> (=) input
            |> ensureEqual expected

    module Nine = // Anagram functions which checks whether or not two provided strings are the same.
        let input = ["fynn"; "nyfn"]
        let expected = true

        let run =
            input
            |> List.map (fun x ->
                x.ToCharArray()
                |> Array.countBy id
                |> Array.sort)
            |> Set.ofList
            |> _.Count
            |> (=) 1
            |> ensureEqual expected

        let run' =
            input
            |> List.map (fun x -> x.ToCharArray() |> Array.sort)
            |> Set.ofList
            |> _.Count
            |> (=) 1
            |> ensureEqual expected

    module Ten = // Count vowels
        let input = "Aloha! My name is Fynn."
        let expected = 6

        let run =
            input.ToCharArray()
            |> Array.where (fun x -> Array.contains (Char.ToLowerInvariant x) [| 'a'; 'e'; 'i'; 'o'; 'u' |])
            |> _.Length
            |> ensureEqual expected

    module Twelve = // Return the most common word in a given string.
        let input = "hello my name is fynn and this is kind of funny. Is this real?"
        let expected = "is"

        let run =
            input.Split(' ')
            |> Array.countBy id
            |> Array.sortByDescending snd
            |> Array.head
            |> fst
            |> ensureEqual expected

    module Fifteen = // Create a function which multiplies all given parameters.
        let input = [2; 3; 4]
        let expected = 24

        let run = input |> List.reduce (*) |> ensureEqual expected

    module Sixteen = // Return the longest word in a given string.
        let input = "Hello my name is Fynn!!"
        let expected = "Hello"

        let run =
            let letters = Array.concat [ [| 'a'..'z' |]; [| 'A'..'Z' |] ]
            let isLetter ch = Array.contains ch letters

            let stripInvalidChars (word: string) =
                word.ToCharArray()
                |> Array.filter isLetter
                |> String

            input.Split(' ')
            |> Array.map stripInvalidChars
            |> Array.sortByDescending _.Length
            |> Array.head
            |> ensureEqual expected

        let run' =
            let validChars = List.concat [ [ 'a'..'z' ]; [ 'A'..'Z' ]; [' '] ]
            let isValidChar ch = List.contains ch validChars

            input.ToCharArray()
            |> Array.filter isValidChar
            |> String
            |> fun x -> x.Split(' ')
            |> Array.sortByDescending _.Length
            |> Array.head
            |> ensureEqual expected

    module Seventeen = // Write a function that accepts a number N as argument and adds all values from N to 1.
        let input = 4
        let expected = 10

        let run =
            [1..input]
            |> List.fold (fun s n -> s + n) 0
            |> ensureEqual expected

    module Eighteen = // Remove duplicated characters from a string.
        let input = "Hello my name is Fynn"
        let expected = "helo mynaisf"

        let run =
            input.ToLowerInvariant()
            |> Array.ofSeq
            |> Array.distinct
            |> String
            |> ensureEqual expected

    module TwentyTwo = // Write a factorial function:
        let input = [4..-1..1]
        let expected = 24

        let run =
            input
            |> List.reduce (*)
            |> ensureEqual expected

Easy.One.run
Easy.Two.run
Easy.Three.run
Easy.Five.run
Easy.Nine.run
Easy.Nine.run'
Easy.Ten.run
Easy.Twelve.run
Easy.Fifteen.run
Easy.Sixteen.run
Easy.Sixteen.run'
Easy.Seventeen.run
Easy.Eighteen.run
Easy.TwentyTwo.run

module Medium =
    module Three = // Reverse each word in a string.
        let input = "Hello my name is Fynn"
        let expected = "olleH ym eman si nnyF"

        let run =
            input.Split ' '
            |> Array.map (fun word -> word.ToCharArray() |> Array.rev |> String)
            |> String.concat " "

    module Eight = // Find the maximum sum of products in two arrays.
        let input = [ [3; 4; 1; 2]; [9; 4; 8; 2] ]
        let expected = 70 // Abbreviated output, eliding the string.

        let run =
            let sortedInput = input |> List.map (fun list -> List.sort list)

            [0..input[0].Length-1]
            |> List.map (fun i -> sortedInput[0][i] * sortedInput[1][i])
            |> List.sum
            |> ensureEqual expected

        let run' =
            input
            |> List.map List.sort
            |> List.transpose
            |> List.map (List.reduce (*))
            |> List.sum
            |> ensureEqual expected

    module Eleven = // Convert a string to "Jaden Case" (TIL: Named after Will Smith's son...)
        let input = "Hello my name is Fynn"
        let expected = "Hello My Name Is Fynn"

        let run =
            input.Split " "
            |> Array.map (fun word -> $"{Char.ToUpperInvariant word[0]}{word[1..]}")
            |> String.concat " "
            |> ensureEqual expected

    module Seventeen = // Check if the parenthesis are balanced.
        let input = "(({}){})"
        let expected = true

        let run () =
            let replace (text: string) = text.Replace("{}", String.Empty).Replace("()", String.Empty)

            let rec check target =
                match replace target with
                | "" -> true
                | x when x.Length = target.Length -> false
                | x -> check x

            check input |> ensureEqual expected

Medium.Three.run
Medium.Eight.run
Medium.Eight.run'
Medium.Eleven.run
Medium.Seventeen.run()
