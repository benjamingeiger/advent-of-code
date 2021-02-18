// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type Satellite = (int * int) * (int * int)

let readLines filePath = System.IO.File.ReadLines filePath
let input =
    readLines "input.txt"
    |> Seq.choose (function
        | Regex "position=<(.*),(.*)> velocity=<(.*),(.*)>" [x; y; vx; vy] -> Some ((int x, int y), (int vx, int vy))
        | s -> printfn "%s" s; None)

(*printfn "%A" input*)

let move n = function | ((x, y), (vx, vy)) -> ((x + n * vx, y + n * vy), (vx, vy))

let moveAll n ss = ss |> Seq.map (move n)

let range ss =
    ss
    |> Seq.map fst
    |> Seq.toList
    |> List.unzip
    |> fun (xs, ys) -> (List.min xs, List.max xs), (List.min ys, List.max ys)

let spread ss =
    range ss
    |> fun ((minx, maxx), (miny, maxy)) -> max (maxx - minx) (maxy - miny)

let tick (prevSpread, ss) =
    let jumpDistance = (prevSpread / 300) + 1
    let ss' = moveAll jumpDistance ss
    let sp' = spread ss'

    if sp' > prevSpread
    then None
    else
        printfn "spread: %A, jumping %A" sp' jumpDistance
        Some ((sp', ss'), (sp', ss'))

let results =
    Seq.unfold tick (spread input, input)
    |> Seq.last

let shift ss =
    let minx = ss |> Seq.map (fun ((x, _), _) -> x) |> Seq.min
    let miny = ss |> Seq.map (fun ((_, y), _) -> y) |> Seq.min

    Seq.map (fun ((x, y), _) -> (x - minx, y - miny)) ss

let draw ss =
    let satellites = shift ss |> Set.ofSeq

    let maxx = satellites |> Seq.map (fun (x, _) -> x) |> Seq.max
    let maxy = satellites |> Seq.map (fun (_, y) -> y) |> Seq.max

    let drawLine r =
        [0..maxx]
        |> List.map (fun c -> if Set.contains (c, r) satellites then "#" else ".")
        |> String.concat ""

    [0..maxy]
    |> List.map drawLine
    |> String.concat "\n"

printfn "%A" (draw (snd results))


