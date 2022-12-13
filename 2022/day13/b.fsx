// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

type Packet = Value of int | List of Packet list | Open

let rawInput = readLines "input.txt" |> List.ofSeq

let rec parsePacket s =
    let step (stack, curNumber) c =
        match c with
        | '[' -> (Open :: stack, None)
        | ',' ->
            match curNumber with
            | Some x -> (Value x :: stack, None)
            | None -> (stack, None) // technically shouldn't happen but still
        | ']' ->
            let stack' =
                match curNumber with
                | Some x -> Value x :: stack
                | None -> stack
            let (before, after) = stack' |> List.splitAt ((stack' |> List.findIndex ((=) Open)) + 1)
            (List (before |> List.rev |> List.tail) :: after, None)
        | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
            match curNumber with
            | Some x -> (stack, Some (x * 10 + (int c - int '0')))
            | None -> (stack, Some (int c - int '0'))
        | ' ' -> (stack, curNumber)
        | _ ->
            printfn "unrecognized character '%c'" c
            (stack, curNumber)

    Seq.fold step ([], None) s |> fst |> List.exactlyOne

type TriState = RightOrder | Indeterminate | WrongOrder

let rec comparePackets (left, right) =
    match left, right with
    | (Open, _) -> failwith "misparsed"
    | (_, Open) -> failwith "misparsed"
    | (Value x, Value y) -> if x < y then RightOrder elif x = y then Indeterminate else WrongOrder
    | (List x, List y) -> compareLists (x, y)
    | (List x, Value y) -> comparePackets (left, List [right])
    | (Value x, List y) -> comparePackets (List [left], right)
and compareLists (left, right) =
    match (left, right) with
    | [], [] -> Indeterminate
    | x::xs, [] -> WrongOrder
    | [], y::ys -> RightOrder
    | x::xs, y::ys ->
        let packetOrder = comparePackets (x, y)
        if packetOrder = Indeterminate then compareLists (xs, ys) else packetOrder

let parseInput rawInput =
    rawInput
    |> Seq.append ["[[2]]"; "[[6]]"]
    |> Seq.filter ((<>) "")
    |> Seq.map parsePacket

let insertionSort values =
    let rec step sorted cur =
        match sorted with
        | [] -> [cur]
        | head :: tail ->
            match comparePackets (cur, head) with
            | WrongOrder -> head :: step tail cur
            | Indeterminate | RightOrder -> cur :: head :: tail

    values |> Seq.fold step []


let run input =
    input
    |> insertionSort
    |> Seq.indexed
    |> Seq.filter (fun (_, v) -> v = List [List [Value 2]] || v = List [List [Value 6]])
    |> Seq.map (fst >> (+) 1)
    |> Seq.fold ( * ) 1

doProcess parseInput run rawInput
