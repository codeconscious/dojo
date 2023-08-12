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

module MyTypes =
    type Species = Human | Mutant | Inhuman | Robot | Android | Alien | Unspecified

    type Powers(powers: string[]) =
        member this.All = powers

    // I decided against this because, for example, humans could have magic-based powers, and
    // aliens might not have any powers.
    // type Species =
    //     | Human
    //     | Mutant of Powers
    //     | Inhuman of Powers
    //     | Robot of Powers
    //     | Android of Powers
    //     | Alien of Powers

    type Person(firstName: string, lastName: string, age: uint8) =
        // member this.Age = age
        member this.FullName = firstName + " " + lastName
        member this.Describe = firstName + " is " + age.ToString() + " years old."

    // TODO: Make powers an Option.
    type SuperPerson(codename: string, species: Species, powers: Powers, civilianIdentity: Person) =
        member this.CivilianIdentity = civilianIdentity
        member this.Describe = codename + " is a(n) " + species.ToString() + " whose real name is " + this.CivilianIdentity.FullName + "."

    type Team(name: string, members: SuperPerson[]) =
        member this.Members = members
        member this.Describe = "The team " + name + " contains " + members.Length.ToString() + " member(s)."
