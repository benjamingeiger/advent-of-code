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

let floodsearch points (minIdx, maxIdx) start =
    let neighbors (x, y) =
        [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]
        |> List.filter (fun (x', y') -> minIdx <= x' && x' <= maxIdx && minIdx <= y' && y' <= maxIdx)

    let distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

    let nearest cur =
        points
        |> Set.toList
        |> List.groupBy (distance cur)
        |> List.sortBy fst
        |> List.head
        |> fun (d, ps) -> if List.length ps > 1 then None else Some (List.head ps)

    let rec step visited contained = function
        | [] -> contained
        | cur :: rest ->
            if Set.contains cur visited
            then
                step visited contained rest
            else
                let newNeighbors = cur |> neighbors |> List.append rest |> List.filter (fun p -> not (Set.contains p visited))
                
                if nearest cur = Some start
                then step (Set.add cur visited) (contained + 1) newNeighbors
                else step (Set.add cur visited) contained rest

    step Set.empty 0 [start]

let getSize padding points =
    let lower = points |> Seq.map (fun (x, y) -> min x y) |> Seq.min
    let upper = points |> Seq.map (fun (x, y) -> max x y) |> Seq.max

    (lower - padding, upper + padding)

let smallGridCounts =
    input |> List.map (fun p -> (p, floodsearch (Set.ofList input) (getSize 10 input) p))

let largeGridCounts =
    input |> List.map (fun p -> (p, floodsearch (Set.ofList input) (getSize 100 input) p))

let results =
    List.append smallGridCounts largeGridCounts
    |> List.groupBy fst
    |> List.filter (fun (_, counts) -> counts.[0] = counts.[1])
    |> List.map (fun (_, counts) -> snd (List.head counts))
    |> List.max

printfn "%A" results
