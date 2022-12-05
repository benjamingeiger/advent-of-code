// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let boxRegex = "\[.\]"
let emptyRegex = "   "
let itemRegex = sprintf "(%s|%s)" boxRegex emptyRegex
let stackRegex = String.concat " " (Seq.init 9 (fun _ -> itemRegex))

let instructionRegex = "move (\d+) from (\d+) to (\d+)"

let matchBox = function
    | "   " -> None
    | s -> Some (s.[1])

let maybeAddBox stack = function
    | Some x -> x :: stack
    | None -> stack

let transposeStacks stacks =
    stacks
    |> List.rev
    |> List.fold (fun stacks cur -> (List.zip stacks cur |> List.map (fun (s, b) -> maybeAddBox s b))) [[];[];[];[];[];[];[];[];[]]


let parseInput rawInput =
    let segments = rawInput |> splitSeq ((=) "")

    let stacks =
        segments
        |> Seq.head
        |> Seq.choose (function
            | Regex stackRegex [box1; box2; box3; box4; box5; box6; box7; box8; box9] ->
                Some ([
                    matchBox box1;
                    matchBox box2;
                    matchBox box3;
                    matchBox box4;
                    matchBox box5;
                    matchBox box6;
                    matchBox box7;
                    matchBox box8;
                    matchBox box9
                ])
            | _ -> None)
        |> Seq.map (List.ofSeq)
        |> List.ofSeq
        |> transposeStacks

    let instructions =
        segments
        |> Seq.skip 1
        |> Seq.head
        |> Seq.choose (function
            | Regex instructionRegex [count; src; dst] ->
                Some (int count, int src - 1, int dst - 1)
            | _ -> None)

    stacks, instructions

let move stacks count src dst =
    let oldSrc = List.item src stacks
    let oldDst = List.item dst stacks

    let moved = oldSrc |> List.take count
    let newSrc = oldSrc |> List.skip count
    let newDst = moved |> List.rev |> (fun x -> List.append x oldDst)

    let returned = stacks |> List.updateAt src newSrc |> List.updateAt dst newDst

    returned

    

let run input =
    let (stacks, instructions) = input

    let finalStacks = instructions |> Seq.fold (fun stacks' (count, src, dst) -> move stacks' count src dst) stacks

    finalStacks
    |> List.map (fun l -> if List.isEmpty l then ' ' else List.head l)
    |> List.map (sprintf "%c")
    |> String.concat ""

doProcess parseInput run rawInput
