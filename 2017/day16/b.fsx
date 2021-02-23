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

let generatePositions moves =
    let originalOrder = "abcdefghijklmnop" |> Seq.toList

    let rec step order = function
        | Spin amount -> List.append (List.skip (List.length order - amount) order) (List.truncate (List.length order - amount) order)
        | Exchange (a, b) -> step order (Partner (List.item a order, List.item b order))
        | Partner (a, b) -> List.map (fun x -> if x = a then b elif x = b then a else x) order

    let dance (seen, pos) =
        let result = List.fold step pos moves

        if List.contains result seen then None else Some (result, (result :: seen, result))

    List.unfold dance ([originalOrder], originalOrder) |> fun xs -> originalOrder :: xs

let positions = generatePositions danceMoves
printfn "%A" (positions |> List.item (1000000000 % List.length positions))

