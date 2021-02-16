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

// borrowed with modification from https://brandewinder.com/2012/04/07/Convex-Hull/
let monotone points =
    let clockwise (x1, y1) (x2, y2) (x3, y3) =
        if (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1) <= 0
        then
            printfn "%A %A %A is clockwise" (x1, y1) (x2, y2) (x3, y3)
            true
        else
            printfn "%A %A %A is not clockwise" (x1, y1) (x2, y2) (x3, y3)
            false

    let rec chain hull = function
        | [] -> hull
        | pt :: rest ->
            match hull with
            | [] | [ _ ] -> chain (pt :: hull) rest
            | cur :: prev :: tail ->
                if clockwise prev cur pt
                then chain (pt :: hull) rest 
                else chain (prev :: tail) rest
    
    match points with
    | [] | [_] -> points
    | _ ->
        let sorted = List.sort points
        let upper = chain [] sorted
        let lower = chain [] (List.rev sorted)
        List.append upper lower

let allPoints = Set.ofList input
let convexHull = Set.ofList (monotone input)
let interior = Set.difference allPoints convexHull

let floodsearch points start =
    printfn "search for neighbors of %A" start

    let neighbors (x, y) = [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]

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
                printfn "already checked %A" cur
                step visited contained rest
            else
                let newNeighbors = cur |> neighbors |> List.append rest |> List.filter (fun p -> not (Set.contains p visited))
                
                if nearest cur = Some start
                then step (Set.add cur visited) (contained + 1) newNeighbors
                else step (Set.add cur visited) contained rest

    step Set.empty 0 [start]

let results =
    interior
    |> Set.map (fun p -> (p, floodsearch allPoints p))

printfn "%A" results
