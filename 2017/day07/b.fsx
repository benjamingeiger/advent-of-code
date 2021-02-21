// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.toList

let weights =
    input
    |> List.choose (function
        | Regex "^(\w+) \((\d+)\)" [name; weight] -> Some ((name, int weight))
        | s ->
            printfn "invalid line [%A]" s
            None)
    |> Map.ofList

let vertices =
    weights
    |> Map.toList
    |> List.map fst
    |> Set.ofList

let edges =
    input
    |> List.map (function
        | Regex "^(\w+).*-> (.*)$" [name; supported] ->
            (supported : string).Split(", ")
            |> Array.toList
            |> List.map (fun s -> (s, name))
        | _ -> [])
    |> List.concat

let supported =
    edges
    |> Set.ofList
    |> Set.map fst

let root = Set.difference vertices supported |> Set.toList |> List.exactlyOne

type Balance = Balanced | Imbalanced
type Program = Program of string * int * Program list * Balance

let rec sumWeights (Program (_, weight, children, _)) =
    weight + List.sumBy sumWeights children

let generateTree weights edges root =
    let rec createProgram name =

        let children =
            edges
            |> List.choose (fun (y, x) -> if x = name then Some y else None)
            |> List.map createProgram

        let isBalanced =
            children
            |> List.map sumWeights
            |> List.distinct
            |> fun ws -> if List.length ws <= 1 then Balanced else Imbalanced

        Program (name, Map.find name weights, children, isBalanced)

    createProgram root

let tree = generateTree weights edges root

(*printfn "%A" tree*)

let rec findHighestImbalanced tree =
    match tree with
    | Program (_, _, _, Balanced) -> None
    | Program (_, _, children, Imbalanced) ->
        match List.choose findHighestImbalanced children with
        | [] -> Some tree
        | [x] -> Some x
        | x :: _ -> failwith "invalid: multiple imbalanced on same level"

let result = match findHighestImbalanced tree with | Some x -> x | None -> failwith "WTF?"

let rectifyBalance (Program (_, weight, children, _)) =
    let childWeights = List.map (fun p -> (sumWeights p, p)) children
   
    let mostCommon = List.countBy fst childWeights |> List.sortByDescending snd |> List.head |> fst

    childWeights
        |> List.map (fun (w, p) -> (w - mostCommon, p))
        |> List.filter (fun (w, _) -> w <> 0)
        |> List.head
        |> fun (w, Program (_, weight, _, _)) -> weight - w

printfn "%A" (rectifyBalance result)
