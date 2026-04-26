#r "nuget: CCFSharpUtils"
#r "nuget: FSharpPlus"
#r "nuget: FsToolkit.ErrorHandling"

open CCFSharpUtils
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Operators

let nextId : State<int,int> =
  monad { // Despite what LLMs will say, there is no `state` CE in F#+.
    let! cur = State.get
    do! State.put (cur + 1)
    return cur
  }

// Helper: generate n ids as a list
let generateN (n:int) : State<int, int list> =
  // create a list of n copies of the same state action, then sequence them
  List.init n (fun _ -> nextId)
  |> sequence

// Example: assign unique ids to a list of values
let assignIds (items: 'a list) : State<int, (int * 'a) list> =
  items
  |> List.map (fun item ->
       monad {
         let! id = nextId
         return (id, item)
       })
  |> sequence

// Running examples
let (ids, finalCounter) = State.run (generateN 5) 100
// ids = [100;101;102;103;104], finalCounter = 105

let (assigned, finalAfterAssign) = State.run (assignIds ["apple"; "banana"; "cherry"]) 1
// assigned = [(1,"apple"); (2,"banana"); (3,"cherry")], finalAfterAssign = 4

// If you only want the generated value and not the final state:
let onlyIds = State.eval (generateN 3) 0
// onlyIds = [0;1;2]

printfn "%A" onlyIds
