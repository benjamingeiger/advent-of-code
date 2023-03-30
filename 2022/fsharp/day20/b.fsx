// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let parseInput rawInput =
    let initialNumbers =
        rawInput
        |> Seq.map int
        |> Seq.map bigint
        |> Seq.map (( * ) 811589153I)
        |> Seq.indexed

    let currentDeque = Deque.ofSeq initialNumbers

    (initialNumbers, currentDeque)

let shiftItem deque (index, offset) =
    let dequeSize = Deque.size deque

    let item', deque' =
        deque
        |> Deque.rotateTo (index, offset)
        |> Deque.dequeueFront

    let deque'' = if offset > 0I then Deque.rotateForward (int ((abs offset) % (bigint (dequeSize - 1)))) deque' else Deque.rotateBack (int ((abs offset) % (bigint (dequeSize - 1)))) deque'

    Deque.enqueueBack item' deque''

let mix initialNumbers count deque =
    let zeroValue = initialNumbers |> Seq.find (fun (_, v) -> v = 0I)

    let numbers =
        initialNumbers
        |> Seq.toList
        |> List.replicate count
        |> List.concat

    let mixed =
        numbers
        |> List.fold shiftItem deque
        |> Deque.rotateTo zeroValue
        |> Deque.toList
        |> List.map snd
        |> fun l -> List.item 1000 l + List.item 2000 l + List.item 3000 l

    mixed

let run (initialNumbers, deque) =
    mix initialNumbers 10 deque

doProcess parseInput run rawInput
