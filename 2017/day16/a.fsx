// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.head |> fun (s : string) -> s.Split(",") |> Array.toList

type DanceMove = Spin of int | Exchange of int * int | Partner of char * char

let parseMove s =
    match s with
    | Regex "^s(\d+)$" [amount] -> Some (Spin (int amount))
    | Regex "^x(\d+)/(\d+)$" [a; b] -> Some (Exchange (int a, int b))
    | Regex "^p(\w)/(\w)$" [a; b] -> Some (Partner (Seq.head a, Seq.head b))
    | s ->
        printfn "invalid dance move [%A]" s
        None

let danceMoves = List.choose parseMove input

printfn "%A" danceMoves

let rec step order = function
    | Spin amount -> List.append (List.skip (List.length order - amount) order) (List.truncate (List.length order - amount) order)
    | Exchange (a, b) -> step order (Partner (List.item a order, List.item b order))
    | Partner (a, b) -> List.map (fun x -> if x = a then b elif x = b then a else x) order

let result = danceMoves |> List.fold step ("abcdefghijklmnop" |> Seq.toList)

printfn "%A" result
