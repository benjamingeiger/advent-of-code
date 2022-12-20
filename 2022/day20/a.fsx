// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let parseInput rawInput =
    let initialNumbers =
        rawInput
        |> Seq.map int
        |> Seq.indexed

    let currentDeque = Deque.ofSeq initialNumbers

    (initialNumbers, currentDeque)

let shiftItem deque (index, offset) =
    let item', deque' =
        deque
        |> Deque.rotateTo (index, offset)
        |> Deque.dequeueFront

    let deque'' = if offset > 0 then Deque.rotateForward (abs offset) deque' else Deque.rotateBack (abs offset) deque'

    Deque.enqueueBack item' deque''

let run input =
    let (initialNumbers, deque) = input

    let zeroValue = initialNumbers |> Seq.find (fun (_, v) -> v = 0)

    let mixed =
        initialNumbers 
        |> Seq.fold shiftItem deque
        |> Deque.rotateTo zeroValue
        |> Deque.toList
        |> List.map snd
        |> fun l -> List.item 1000 l + List.item 2000 l + List.item 3000 l

    mixed

doProcess parseInput run rawInput
