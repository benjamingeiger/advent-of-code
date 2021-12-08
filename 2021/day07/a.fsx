// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> Seq.head |> commaInts

let calculateFuel crabs pos =
    crabs
    |> Seq.map ((-) pos >> abs)
    |> Seq.sum

let result =
    [(Seq.min input)..(Seq.max input)]
    |> Seq.map (calculateFuel input)
    |> Seq.min

printfn "%A" result
