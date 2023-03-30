// vim: set et ts=4 sw=4 list :

open System
open Checked

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let parseOperation = function
    | Regex "old \+ (\d+) *" [v] -> fun x -> x + bigint (int v)
    | Regex "old \* (\d+) *" [v] -> fun x -> x * bigint (int v)
    | Regex "old \* old *" [] -> fun x -> ((x * x) % (2I * 3I * 5I * 7I * 11I * 13I * 17I * 19I * 23I))
    | s ->
        printfn "failed to parse operation %A" s
        id

let parseTest v = fun x -> x % v = 0I

let parseInput rawInput =
    let monkeyTexts = rawInput |> splitSeq ((=) "") |> Seq.map (String.concat "")

    monkeyTexts
    |> Seq.choose (function
        | Regex "Monkey (\d+): *Starting items: ([\d, ]+)Operation: new = (.*)Test: divisible by (\d+) *If true: throw to monkey (\d+) *If false: throw to monkey (\d+)" [monkeyNo; items; operation; test; ifTrue; ifFalse] ->
            Some ((int monkeyNo, parseOperation operation, parseTest (bigint (int test)), int ifTrue, int ifFalse), [for item in commaInts items do yield (int monkeyNo, bigint item)])
        | x ->
            printfn "invalid monkey %A" x
            None)
    |> List.ofSeq
    |> List.unzip
    |> fun (x, y) -> (x, List.collect id y)

let doTurn (inspectionCounts, items) monkey =
    let (monkeyNo, operation, test, ifTrue, ifFalse) = monkey

    let (ourItems, theirItems) =
        items
        |> List.partition (fun (monkeyNo', _) -> monkeyNo = monkeyNo')

    let inspectionCount = List.length ourItems

    let itemsThrown =
        ourItems
        |> List.map (snd >> operation)
        |> List.map (fun worry -> worry % (2I * 3I * 5I * 7I * 11I * 13I * 17I * 19I * 23I))
        |> List.map (fun worry ->
            if (test worry) then
                (ifTrue, worry)
            else
                (ifFalse, worry))

    ((monkeyNo, inspectionCount) :: inspectionCounts, List.append theirItems itemsThrown)

let doRound monkeys (inspectionCounts, items) _ =
    monkeys |> Seq.fold doTurn (inspectionCounts, items)

let run input =
    let (monkeys, items) = input

    [1..10000]
    |> List.fold (doRound monkeys) ([], items)
    |> fst
    |> List.groupBy fst
    |> List.map (fun (i, xs) -> (i, xs |> List.map snd |> List.sum))
    |> List.sortByDescending snd
    |> List.take 2
    |> List.map snd
    |> List.map bigint
    |> List.fold ( * ) 1I

doProcess parseInput run rawInput
