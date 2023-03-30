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

let step ((headR, headC), ((tailR, tailC) :: tailPoses)) = function
    | Up ->
        let newHeadR = headR - 1
        let newTail = adjustTail (newHeadR, headC) (tailR, tailC)

        ((newHeadR, headC), (newTail :: (tailR, tailC) :: tailPoses))
    | Down ->
        let newHeadR = headR + 1
        let newTail = adjustTail (newHeadR, headC) (tailR, tailC)

        ((newHeadR, headC), (newTail :: (tailR, tailC) :: tailPoses))
    | Left ->
        let newHeadC = headC - 1
        let newTail = adjustTail (headR, newHeadC) (tailR, tailC)

        ((headR, newHeadC), (newTail :: (tailR, tailC) :: tailPoses))
    | Right ->
        let newHeadC = headC + 1
        let newTail = adjustTail (headR, newHeadC) (tailR, tailC)

        ((headR, newHeadC), (newTail :: (tailR, tailC) :: tailPoses))

let run input =
    input
    |> Seq.fold step ((0, 0), [(0, 0)])
    |> snd
    |> Set.ofSeq
    |> Set.count

doProcess parseInput run rawInput
