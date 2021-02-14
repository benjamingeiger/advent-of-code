// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.sort |> Seq.cache

type Event =
    | ShiftChange of int
    | Sleep
    | Wake
type Record = Record of int * Event

let records =
    input
    |> Seq.choose (function
        | Regex ":(\d\d)] Guard #(\d+) begins shift" [minute; guard] -> Some (Record (int minute, ShiftChange (int guard)))
        | Regex ":(\d\d)] falls asleep" [minute] -> Some (Record (int minute, Sleep))
        | Regex ":(\d\d)] wakes up" [minute] -> Some (Record (int minute, Wake))
        | s ->
            printfn "Invalid log entry [%A]" s
            None)

let recordNaps records =
    let addNap guards currentGuard sleptAt wokeAt =
        match (Map.tryFind currentGuard guards) with
        | Some sleeps -> Map.add currentGuard ((sleptAt, wokeAt)::sleeps) guards
        | None -> Map.add currentGuard [(sleptAt, wokeAt)] guards

    let step (currentGuard, sleptAt, guards) = function
        | Record (minute, ShiftChange guard) ->
            match sleptAt with
            | Some t -> (guard, None, addNap guards currentGuard t minute)
            | None -> (guard, None, guards)
        | Record (minute, Sleep) ->
            match sleptAt with
            | Some t -> failwith "fell asleep twice?"
            | None -> (currentGuard, Some minute, guards)
        | Record (minute, Wake) ->
            match sleptAt with
            | Some t -> (currentGuard, None, addNap guards currentGuard t minute)
            | None -> failwith "woke up twice?"

    Seq.fold step (0, None, Map.empty) records
    |> (fun (_, _, naps) -> naps)

let naps = recordNaps records

let biggestMinute ns =
    [0..59]
    |> List.map (fun i -> (Seq.filter (fun (x, y) -> x <= i && i < y)) ns)
    |> List.map Seq.length
    |> List.indexed
    |> List.maxBy snd

let napMinutes =
    naps
    |> Map.toSeq
    |> Seq.map (fun (g, ns) -> (g, biggestMinute ns))
    |> Seq.maxBy (fun (g, (m, c)) -> c)

printfn "%A" napMinutes

let (guard, (minute, count)) = napMinutes
printfn "%A" (guard * minute)
