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
        | Regex "(\d+), (\d+)" [x; y] -> Some (int x, int y)
        | s ->
            printfn "invalid point %A" s
            None)
    |> List.ofSeq

let floodsearch points maxDistance start =
    let neighbors (x, y) =
        [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]

    let distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)
    let totalDistance (x, y) = points |> Seq.map (distance (x, y)) |> Seq.sum

    let rec step visited contained = function
        | [] -> contained
        | cur :: rest ->
            if Set.contains cur visited
            then
                step visited contained rest
            else
                let newNeighbors = cur |> neighbors |> List.append rest // |> List.filter (fun p -> not (Set.contains p visited))
                
                if totalDistance cur < maxDistance
                then step (Set.add cur visited) (contained + 1) newNeighbors
                else step (Set.add cur visited) contained rest

    step Set.empty 0 [start]

let center =
    input
    |> Seq.fold (fun (a1, a2) (x, y) -> (a1 + x, a2 + y)) (0, 0)
    |> fun (x, y) -> x / Seq.length input, y / Seq.length input

let result = floodsearch input 10000 center

printfn "%A" result
