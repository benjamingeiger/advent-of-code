// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt"

let parsedInput =
    input
    |> Seq.choose (function
        | Regex "(\d+),(\d+) -> (\d+),(\d+)" [r1;c1;r2;c2] ->
            Some ((int r1, int c1), (int r2, int c2))
        | _ -> None
    )

let generateLine ((r1, c1), (r2, c2)) =
    let order x1 x2 = (min x1 x2, max x1 x2)

    let range from to_ =
        (if from > to_ then seq { from .. -1 .. to_ } else seq { from .. to_ }) |> Seq.toList

    if r1 = r2 then
        let c1', c2' = order c1 c2
        [c1'..c2'] |> List.map (fun c -> (r1, c))
    elif c1 = c2 then
        let r1', r2' = order r1 r2
        [r1'..r2'] |> List.map (fun r -> (r, c1))
    else 
        let rs = range r1 r2
        let cs = range c1 c2
        List.zip rs cs
        

let points =
    parsedInput
    |> Seq.collect generateLine
    |> Seq.countBy id
    |> Seq.filter (fun (point, count) -> count > 1)
    |> Seq.length

printfn "%A" points
