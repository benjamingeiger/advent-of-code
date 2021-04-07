// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let input =
    readLines "input.txt"
    |> Seq.choose (function
        | Regex "^(-?\d+),(-?\d+),(-?\d+),(-?\d+)$" [xt; yt; zt; wt] -> Some (int xt, int yt, int zt, int wt)
        | x ->
            eprintfn "Invalid coordinates %A" x
            None)

let manhattanDistance (x1, y1, z1, w1) (x2, y2, z2, w2) =
    abs(x2 - x1) + abs(y2 - y1) + abs(z2 - z1) + abs(w2 - w1)

let rec getCanonical map coord =
    match Map.tryFind coord map with
    | Some coord' when coord' = coord -> Some coord
    | Some coord' -> getCanonical map coord'
    | None -> None

let join f map coord =
    let matchingConstellations =
        Map.filter f map
        |> Map.toList
        |> List.choose (fun (k, v) -> (getCanonical map k))
        |> List.distinct

    let updateConstellation origCanonical map newCanonical =
        Map.add newCanonical origCanonical map

    List.fold (updateConstellation coord) (Map.add coord coord map) matchingConstellations

let step map (coord: int * int * int * int) =
    join (fun k v -> manhattanDistance coord k <= 3) map coord

let constellationMap = input |> Seq.fold step Map.empty

let constellations =
    constellationMap
    |> Map.toList
    |> List.map (fun (k, v) -> getCanonical constellationMap k)
    |> List.distinct

printfn "%A" (List.length constellations)
