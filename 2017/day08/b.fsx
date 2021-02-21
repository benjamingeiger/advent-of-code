// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let parseLine = function
    | Regex "^(\w+) (inc|dec) (-?\d+) if (\w+) (\W+) (-?\d+)$" [destination; operator; operand; source; comparison; target] ->
        Some (destination, operator, int operand, source, comparison, int target)
    | s ->
        printfn "invalid instruction [%A]" s
        None

let instructions = Seq.choose parseLine input

let get register map = match Map.tryFind register map with | Some v -> v | None -> 0

let inc register value map = Map.add register ((get register map) + value) map

let dec register value map = inc register (-1 * value) map

let run instructions =
    let update registers (destination, operator, operand, source, comparison, target) =
        let operator' =
            match operator with
            | "inc" -> inc
            | "dec" -> dec
            | s -> failwith (sprintf "invalid operator [%A]" s)

        let shouldRun =
            match comparison with
            | "==" -> (get source registers) = target
            | "!=" -> (get source registers) <> target
            | "<" -> (get source registers) < target
            | ">" -> (get source registers) > target
            | "<=" -> (get source registers) <= target
            | ">=" -> (get source registers) >= target
            | s -> failwith (sprintf "invalid comparison [%A]" s)

        if shouldRun
        then operator' destination operand registers
        else registers

    Seq.scan update Map.empty instructions

let result = run instructions |> Seq.tail |> Seq.map (fun m -> m |> Map.toList |> List.map snd |> List.max) |> Seq.max

printfn "%A" result
