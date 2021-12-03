// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> Seq.cache

let inputCount = Seq.length input
let wordLength = Seq.length (Seq.head input)

let extractBit pos (s : string) = s.[pos]

let countOnes pos strs =
    strs
    |> Seq.map (extractBit pos)
    |> Seq.filter (fun c -> c = '1')
    |> Seq.length

let gammaBit strs pos =
    strs
    |> countOnes pos
    |> fun n -> if n * 2 > inputCount then 1 else 0

let gamma =
    [0..(wordLength-1)]
    |> Seq.map (gammaBit input)
    |> Seq.fold (fun acc e -> acc * 2 + e) 0

let epsilon =
    [0..(wordLength-1)]
    |> Seq.map (gammaBit input)
    |> Seq.map (fun n -> if n = 1 then 0 else 1)
    |> Seq.fold (fun acc e -> acc * 2 + e) 0

printfn "%A" gamma
printfn "%A" epsilon
printfn "%A" (gamma * epsilon)
