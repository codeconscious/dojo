module Quadratic

type EquationType =
    | NotQuadratic
    | NoRoots
    | OneRoot of positive : float
    | TwoRoots of positive : float * negative : float

let calculate a b c =
    let discrimant =
        if a = 0.0 then None
        else Some((pown b 2) - (4.0 * a * c));
    let rootPos discrimant a b = (-b + sqrt discrimant) / (2.0 * a);
    let rootNeg discrimant a b = (-b - sqrt discrimant) / (2.0 * a);

    let result = match discrimant with
                    | None -> NotQuadratic
                    | Some d when d < 0 -> NoRoots
                    | Some d when d = 0 -> OneRoot (rootPos d a b)
                    | Some d -> TwoRoots((rootPos d a b), (rootNeg d a b))

    match result with
    | NotQuadratic -> "The equation is not quadratic (because `a` is zero)!"
    | NoRoots -> "The equation has no real roots."
    | OneRoot r -> sprintf "The equation has one root: %f." r
    | TwoRoots (r1, r2) -> sprintf "The equation has two roots: %f and %f." r1 r2
