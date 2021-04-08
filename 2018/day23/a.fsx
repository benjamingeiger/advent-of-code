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
        | Regex @"^pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)$" [x; y; z; r] ->
            Some ((int x, int y, int z), int r)

        | s ->
            eprintfn "invalid bot [%A]" s
            None)

let strongestBot =
    input |> Seq.maxBy snd

let botDistance ((x1, y1, z1), _) ((x2, y2, z2), _) =
    abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

(*printfn "%A" strongestBot*)

input
|> Seq.map (botDistance strongestBot)
|> Seq.filter ((>=) (snd strongestBot))
|> Seq.length
|> printfn "%A"
