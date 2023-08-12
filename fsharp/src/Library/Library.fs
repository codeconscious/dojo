module Library

open System.Text.Json

let getJson value =
    let json = JsonSerializer.Serialize(value)
    value, json

let everyTen integers =
    integers
    |> List.filter(fun i -> i % 10 = 0)

let every integers by =
    integers
    |> List.filter(fun i -> i % by = 0)

let sumTo target =
        [ for i in 1..99 do
            for j in 1..99 do
                if (i + j = target) then
                    yield (i, j) ]

let sumTuples tuples =
    tuples
    |> List.map(fun (x,y) -> x + y)
