// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> Seq.head |> commaInts

let calculateFuel crabs pos =
    let offset x y = (abs (x - y))
    let fuel x y = (offset x y) * ((offset x y) + 1) / 2

    crabs
    |> Seq.map (fuel pos)
    |> Seq.sum

let result =
    [(Seq.min input)..(Seq.max input)]
    |> Seq.map (calculateFuel input)
    |> Seq.min

printfn "%A" result
