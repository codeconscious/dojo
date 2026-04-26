type State<'s, 'a> = State of ('s -> 'a * 's)

module State =
    let run (State f) state = f state

    let returnValue a = State(fun s -> (a, s))

    let bind (State f) k = State(fun s ->
        let (a, s') = f s
        run (k a) s'
    )

    let map f (State g) = State(fun s ->
        let (a, s') = g s
        (f a, s')
    )

    let get = State(fun s -> (s, s))

    let put s = State(fun _ -> ((), s))

    let modify f = State(fun s -> ((), f s))

type StateBuilder() =
    member _.Return(a) = State.returnValue a
    member _.Bind(m, k) = State.bind m k
    member _.Let(a, k) = k a

module Example1 =
    open State

    let state = StateBuilder()

    let increment = State.modify ((+) 1)

    let incrementTwice =
        state {
            do! increment
            do! increment
        }

    let ff () = State.run incrementTwice 0  // ((), 2)
