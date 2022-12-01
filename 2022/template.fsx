// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> List.ofSeq

let parseInput input =
    input



let result =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let parsed = parseInput input

    let returnValue =
        parsed

    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds

    returnValue

printfn "%A" result
