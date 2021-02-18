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

let move n = function | ((x, y), (vx, vy)) -> ((x + n * vx, y + n * vy), (vx, vy))

let moveAll n (t, ss) = (t + n, ss |> Seq.map (move n))

let range ss =
    ss
    |> Seq.map fst
    |> Seq.toList
    |> List.unzip
    |> fun (xs, ys) -> (List.min xs, List.max xs), (List.min ys, List.max ys)

let spread ss =
    range ss
    |> fun ((minx, maxx), (miny, maxy)) -> max (maxx - minx) (maxy - miny)

let tick (prevSpread, (t, ss)) =
    let jumpDistance = (prevSpread / 300) + 1
    let (t', ss') = moveAll jumpDistance (t, ss)
    let sp' = spread ss'

    if sp' > prevSpread
    then None
    else
        Some ((sp', (t', ss')), (sp', (t', ss')))

let (_, (t, _)) =
    Seq.unfold tick (spread input, (0, input))
    |> Seq.last

printfn "%A" t
