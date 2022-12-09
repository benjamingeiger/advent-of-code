// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

type Move =
    | Up
    | Down
    | Left
    | Right

let parseInput rawInput =
    rawInput
    |> Seq.collect (fun (s : string) ->
        let distance = s.[2..] |> int

        match s.[0] with
        | 'U' -> List.replicate distance Up
        | 'R' -> List.replicate distance Right
        | 'D' -> List.replicate distance Down
        | 'L' -> List.replicate distance Left
        | _ -> [])
    |> List.ofSeq

let adjustTail (headR, headC) (tailR, tailC) =
    let columnDist = tailC - headC
    let rowDist = tailR - headR

    if abs columnDist <= 1 && abs rowDist <= 1 then (tailR, tailC)
    else
        let newTailR = if rowDist = 0 then tailR else (tailR - rowDist / (abs rowDist))
        let newTailC = if columnDist = 0 then tailC else (tailC - columnDist / (abs columnDist))

        (newTailR, newTailC)

let step (rope, tailPoses) direction =
    let (headR, headC) = List.head rope
    let rest = List.tail rope

    let newHead =
        match direction with
        | Up -> headR - 1, headC
        | Down -> headR + 1, headC
        | Left -> headR, headC - 1
        | Right -> headR, headC + 1

    let newRope =
        List.fold (fun ((endR, endC), newRope') oldKnot ->
            let newKnot = adjustTail (endR, endC) oldKnot
            (newKnot, newKnot :: newRope')) (newHead, [newHead]) rest
        |> snd

    ((List.rev newRope), ((List.head newRope) :: tailPoses))

let run input =
    input
        |> Seq.fold step ((List.replicate 10 (0, 0)), [(0, 0)])
        |> snd
        |> Set.ofSeq
        |> Set.count

doProcess parseInput run rawInput
