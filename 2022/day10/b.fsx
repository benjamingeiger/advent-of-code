// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

type Instruction =
    | NoOp
    | AddX of int

let rawInput = readLines "input.txt" |> List.ofSeq

let parseInput rawInput =
    rawInput
    |> Seq.choose (function
        | Regex "noop" [] -> Some NoOp
        | Regex "addx (-?\d+)" [value] -> Some (AddX (int value))
        | _ -> None)

let doStep (i, x) = function
    | NoOp -> i + 1, x
    | AddX value -> i + 2, x + value

let lookup x map =
    Map.tryFind x map |> Option.orElse (Map.tryFind (x - 1) map) |> Option.defaultValue 0

let rowNumber i = List.init 40 (fun x -> x, x + 40 * i)

let drawRow values i =
    let pixels = rowNumber i

    pixels
    |> Seq.map (fun (column, cycle) ->
        let value = lookup (cycle + 1) values
        if abs (column - value) <= 1 then "#" else ".")
    |> String.concat ""

let run input =
    let values =
        input
        |> Seq.scan doStep (1, 1)
        |> Map.ofSeq

    [0; 1; 2; 3; 4; 5]
    |> Seq.map (drawRow values)
    |> String.concat "\n"
    |> printfn "%s"

    "see above"

doProcess parseInput run rawInput
