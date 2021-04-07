// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic

let depth = 3198I
let targetx = 12
let targety = 757

let memo = new Dictionary<int * int, bigint>()

let rec geologicIndex (x, y) =
    let go' () =
        match (x, y) with
        | (0, 0) -> 0I
        | (x', y') when (x', y') = (targetx, targety) -> 0I
        | (_, 0) -> ((bigint x) * 16807I) % 20183I
        | (0, _) -> ((bigint y) * 48271I) % 20183I
        | (_, _) -> (erosionLevel (x - 1, y) * erosionLevel (x, y - 1)) % 20183I

    match memo.TryGetValue((x, y)) with
    | true, v -> v
    | false, _ ->
        let v = go' ()
        memo.Add((x, y), v)
        v

and erosionLevel (x, y) =
    let gi = geologicIndex (x, y)
    (gi + depth) % 20183I

let regionType (x, y) = erosionLevel (x, y) % 3I

List.allPairs [0..targetx] [0..targety]
|> List.map regionType
|> List.sum
|> printfn "%A"
