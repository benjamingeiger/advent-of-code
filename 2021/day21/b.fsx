// vim: set et ts=4 sw=4 list :

open System

open System.Collections.Generic

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> List.ofSeq

let parseInput input =
    input
    |> Seq.choose (function
        | Regex "^Player . starting position: (\d+)$" [pos] -> Some (int pos)
        | _ -> None)
    |> (fun xs -> (Seq.item 0 xs, Seq.item 1 xs))

let rolls = [ for a in 1 .. 3 do
              for b in 1 .. 3 do
              for c in 1 .. 3 do
                a + b + c ]

let compute goal =
    let dict = new Dictionary<(int * (int * int) * (int * int)), (bigint * bigint)>()

    let rec memoizedFunc input =

        match dict.TryGetValue(input) with
        | true, x -> x
        | false, _ ->
            let answer = doComputation input
            dict.Add(input, answer)
            answer

    and doComputation (turn, (pos1, score1), (pos2, score2)) =

        let advance pos roll =
            let newPos = (pos + roll) % 10
            if newPos = 0 then 10 else newPos

        let combine (a, b) (c, d) = (a + c, b + d)

        if score1 >= goal then (1I, 0I)
        elif score2 >= goal then (0I, 1I)

        elif turn % 2 = 0 then
            rolls
            |> List.map (fun roll -> memoizedFunc (turn + 1, (advance pos1 roll, score1 + (advance pos1 roll)), (pos2, score2)))
            |> List.reduce combine

        else
            rolls
            |> List.map (fun roll -> memoizedFunc (turn + 1, (pos1, score1), (advance pos2 roll, score2 + (advance pos2 roll))))
            |> List.reduce combine

    (memoizedFunc, dict)

let result =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let (start1, start2) = parseInput input

    let computeFunc, dict = compute 21

    let returnValue =
        computeFunc (0, (start1, 0), (start2, 0))
        |> fun (a, b) -> max a b

    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds

    returnValue

printfn "%A" result
