// vim: set et ts=4 sw=4 list :

open System

type Acre =
    | Open
    | Trees
    | Lumberyard
    | Chasm // outside the map

let readLines filePath = System.IO.File.ReadLines filePath
let input =
    readLines "input.txt"
    |> Seq.map (
        Seq.map (function
        | '.' -> Open
        | '|' -> Trees
        | '#' -> Lumberyard
        | _ -> Chasm))
    |> Seq.map (Array.ofSeq)
    |> Array.ofSeq

let getDimensions input = (Seq.length input, Seq.length (Seq.head input))
let (rows, cols) = getDimensions input

let initialGrid = Array2D.init rows cols (fun r c -> input.[r].[c])

let tryGet grid (r, c) =
    try
        Array2D.get grid r c
    with
        | :? System.IndexOutOfRangeException -> Chasm

let countOf acreType xs =
    xs
    |> Seq.filter ((=) acreType)
    |> Seq.length

let tick rows cols grid =
    let neighbors r c =
        [(r - 1, c - 1); (r - 1, c); (r - 1, c + 1);
         (r,     c - 1);             (r,     c + 1);
         (r + 1, c - 1); (r + 1, c); (r + 1, c + 1)]

    let newGrid = Array2D.init rows cols (fun r c ->
        let ns = neighbors r c |> List.map (tryGet grid)

        match tryGet grid (r, c) with
        | Open ->
            if countOf Trees ns >= 3 then Trees else Open
        | Trees ->
            if countOf Lumberyard ns >= 3 then Lumberyard else Trees
        | Lumberyard ->
            if countOf Lumberyard ns >= 1 && countOf Trees ns >= 1 then Lumberyard else Open
        | Chasm -> Chasm)

    newGrid

let result = [1..10] |> Seq.fold (fun grid _ -> tick rows cols grid) initialGrid

let resourceValue grid =
    let resourceCounts = grid |> Seq.cast<Acre> |> Seq.countBy id |> Map.ofSeq

    Map.find Trees resourceCounts * Map.find Lumberyard resourceCounts

printfn "%A" (resourceValue result)
