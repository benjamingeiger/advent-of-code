// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt"

let edges =
    input
    |> Seq.choose (function
        | Regex "^([A-Za-z]+)-([A-Za-z]+)$" [fromNode; toNode] ->
            Some (fromNode, toNode)
        | x ->
            printfn "Unparseable line? [%s]" x
            None)
    |> Seq.collect (fun (x, y) -> [(x, y); (y, x)])
    |> Seq.groupBy fst
    |> Seq.map (fun (from, tos) -> (from, Set.ofSeq (Seq.map snd tos)))
    |> Map.ofSeq

let isLower = String.forall Char.IsLower

let findAllPaths start goal edges =
    let rec go = function
        | cur :: restOfPath when cur = goal ->
            Set.singleton (cur :: restOfPath)
        | cur :: restOfPath ->
            Map.find cur edges
            |> fun s -> Set.difference s (Set.ofSeq (List.filter isLower restOfPath))
            |> Set.map (fun next -> go (next :: cur :: restOfPath))
            |> Set.unionMany
        | [] -> failwith "this should never happen"

    go [start]

let result =
    findAllPaths "start" "end" edges
    (*|> Set.map List.rev*)
    |> Set.count

printfn "%A" result
