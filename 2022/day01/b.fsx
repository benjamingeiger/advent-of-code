// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> List.ofSeq

let parseInput input =
    input
    |> splitSeq ((=) "")
    |> Seq.map (Seq.map int)

let result =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let parsed = parseInput input

    let returnValue =
        parsed
        |> Seq.map (Seq.sum)
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.sum

    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds

    returnValue

printfn "%A" result
