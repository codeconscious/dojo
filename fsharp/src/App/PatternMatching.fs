module PatternMatching

module WithFunction =
    let filterNumbers num =
        match num with
            | 1 | 2 | 3 -> printfn "Found 1, 2, or 3!"
            | a -> printfn "%d" a

    let filterNumbers' =  // the paramater and `match num with` are combined
        function | 1 | 2 | 3 -> printfn "Found 1, 2, or 3!"
                 | a -> printfn "%d" a

    let two = 2
    filterNumbers' two
