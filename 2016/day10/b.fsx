// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let toposort edges =
    let findStarts es =
        let fromVertices = es |> Seq.map fst |> Set.ofSeq
        let toVertices = es |> Seq.map snd |> Set.ofSeq

        Set.difference fromVertices toVertices |> Set.toList

    let removeOutgoingEdges v es =
        List.filter (fun (a, b) -> a <> v) es

    let rec go visited es =
        match es with
        | [] -> visited
        | _ ->
            match findStarts es with
            | [] -> failwith "whoops, found a cycle"
            | x :: xs -> go (x :: visited) (removeOutgoingEdges x es)

    go [] edges |> List.rev

type Destination = Output of int | Bot of int
type Instruction = Value of int * Destination | Transfer of int * Destination * Destination

let botPattern = @"^bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)$"
let valuePattern = @"^value (\d+) goes to bot (\d+)$"

let parseLine s =
    match Regex.Match(s, botPattern) with
    | m when m.Success ->
        let dest1 = if m.Groups.[2].Value = "output" then Output (int m.Groups.[3].Value) else Bot (int m.Groups.[3].Value)
        let dest2 = if m.Groups.[4].Value = "output" then Output (int m.Groups.[5].Value) else Bot (int m.Groups.[5].Value)

        Transfer (int m.Groups.[1].Value, dest1, dest2)
    | _ ->
        match Regex.Match(s, valuePattern) with
        | m when m.Success -> Value (int m.Groups.[1].Value, Bot (int m.Groups.[2].Value))
        | _ -> failwith (sprintf "invalid instruction %s" s)

let instructions = Seq.map parseLine input |> List.ofSeq

(*printfn "%A" instructions*)

type BotState = Empty | Half of int | Full of int * int | Bucket of int

let transferChip state inst = 
    let updateBot dest newValue (state : Map<int, BotState>) =
        match dest with
        | Output bin -> Map.add (10000 + bin) (Bucket newValue) state
        | Bot botNum ->
            match state.[botNum] with
            | Empty -> Map.add botNum (Half newValue) state
            | Half other -> Map.add botNum (Full (newValue, other)) state
            | Full (low, high) -> state
            | Bucket _ -> failwith "pulling from a bucket!"

    match inst with
    | Value (value, dest) -> updateBot dest value state
    | Transfer (botNum, lowDest, highDest) ->
        match state.[botNum] with
        | Bucket _ -> failwith "Trying to pull from a bucket"
        | Empty -> failwith "Trying to pull from an empty bot"
        | Half _ -> failwith "Trying to pull from a half-full bot"
        | Full (v1, v2) ->
            let (low, high) = if v1 < v2 then (v1, v2) else (v2, v1)

            state
            |> updateBot lowDest low
            |> updateBot highDest high
 
let extractEdges inst =
    match inst with
    | Value (_, Output _) -> []
    | Value (_, Bot botNum) -> [(-1, botNum)]
    | Transfer (botNum, Output _, Output _) -> []
    | Transfer (botNum, Bot dest1, Output _) -> [(botNum, dest1)]
    | Transfer (botNum, Bot dest1, Bot dest2) -> [(botNum, dest1); (botNum, dest2)]
    | Transfer (botNum, Output _, Bot dest2) -> [(botNum, dest2)]

let sortedTransfers =
    let isBotInstruction botNum inst =
        match inst with
        | Value _ -> false
        | Transfer (b, _, _) -> b = botNum

    instructions
    |> List.ofSeq
    |> List.collect extractEdges
    |> toposort
    |> List.collect (fun v -> List.filter (isBotInstruction v) instructions)

let sortedInsts =
    let isValue inst =
        match inst with
        | Value _ -> true
        | Transfer _ -> false

    instructions
    |> List.filter isValue
    |> fun vs -> List.append vs sortedTransfers

(*printfn "%A" sortedInsts*)

let allBots =
    instructions
    |> List.collect extractEdges
    |> List.map snd
    |> List.distinct
    |> List.map (fun b -> (b, Empty))
    |> Map.ofList

let populatedBots = List.fold transferChip allBots sortedInsts

(*printfn "%A" populatedBots*)

let getBucketValue bot =
    match bot with
    | Bucket x -> x
    | _ -> 1

populatedBots
|> Map.toList
|> List.filter (fun (k, v) -> k = 10000 || k = 10001 || k = 10002)
|> List.map (snd >> getBucketValue)
|> List.fold ( * ) 1
|> printfn "%A"
