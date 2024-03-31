module ComputationExpressions

// Modified from example at https://fsharpforfunandprofit.com/posts/computation-expressions-intro.
type LoggingBuilder() =
    let log p = printfn "expression is %A" p

    member this.Bind(x, f) =
        log x
        f x

    member this.Return(x) =
        x

module Practice1 =
// The exercise at https://fsharpforfunandprofit.com/posts/computation-expressions-bind/:
    open System

    let strToInt (str:string) =
        match Int32.TryParse(str) with
        | true, int -> Some int
        | false, _ -> None

    type AdditionBuilder() =
        member this.Bind(m, f) = Option.bind f m
        member this.Return(x) = Some x

    let workflow = new AdditionBuilder()

    let stringAddWorkflow x y z =
        workflow
            {
            let! a = strToInt x
            let! b = strToInt y
            let! c = strToInt z
            return a + b + c
            }

    // Tests:
    let good = stringAddWorkflow "12" "3" "2"
    let bad = stringAddWorkflow "12" "xyz" "2"

    let printResult o =
        match o with
        | Some i -> sprintf "Output: %i" i
        | None -> sprintf "None!"
