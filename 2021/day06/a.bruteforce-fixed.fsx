// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> Seq.head |> (fun (s : string) -> s.Split(",")) |> Seq.map int |> Seq.toList

printfn "%A" input

let spawnFish school =
    let execute (newSchool, addedFish) = function
        | 0 -> (6 :: newSchool, 8 :: addedFish)
        | n -> ((n - 1) :: newSchool, addedFish)

    school
    |> Seq.toList
    |> Seq.fold execute ([], [])
    |> fun (a, b) -> b @ a
    |> List.rev

let spawnFishRepeatedly count school =
    let execute school _ = spawnFish school

    [1..count]
    |> Seq.fold execute school

let result =
    input
    |> spawnFishRepeatedly 80
    |> List.length

printfn "%A" result
