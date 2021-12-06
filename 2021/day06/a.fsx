// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> Seq.head |> (fun (s : string) -> s.Split(",")) |> Seq.map int

let inputMap =
    input
    |> Seq.countBy id
    |> Map.ofSeq

let spawnFish school =
    school
    |> Map.toSeq
    |> Seq.collect (fun (days, count) ->
        match days with
        | 0 -> [(6, count); (8, count)]
        | n -> [(n - 1, count)])
    |> Seq.groupBy fst
    |> Seq.map (fun (k, fish) -> fish |> Seq.map snd |> Seq.sum |> fun n -> (k, n))
    |> Map.ofSeq

let rec spawnFishRepeatedly count school =
    match count with
    | 0 -> school
    | n -> spawnFishRepeatedly (n - 1) (spawnFish school)

let result =
    inputMap
    |> spawnFishRepeatedly 80
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.sum

printfn "%A" result
