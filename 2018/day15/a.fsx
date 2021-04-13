// vim: set et ts=4 sw=4 list :

open System

type Cell = Empty | Goblin of int | Elf of int | Wall

let initHitPoints = 200

let readLines filePath = System.IO.File.ReadLines filePath
let input =
    readLines "input.txt"
    |> Seq.map Seq.indexed
    |> Seq.indexed
    |> Seq.collect (fun (r, cs) -> cs |> Seq.map (fun (c, v) -> ((r, c), v)))
    |> Seq.choose (function
        | ((r, c), '#') -> None
        | ((r, c), '.') -> Some ((r, c), Empty)
        | ((r, c), 'G') -> Some ((r, c), Goblin initHitPoints)
        | ((r, c), 'E') -> Some ((r, c), Elf initHitPoints)
        | x ->
            eprintfn "Unknown input [%A]" x
            None)
    |> Map.ofSeq

let dumpMap map =
    let toChar = function
        | Empty -> '.'
        | Goblin _ -> 'G'
        | Elf _ -> 'E'
        | Wall -> ' '

    let maxCol =
        map
        |> Map.toSeq
        |> Seq.map (fun ((_, c), _) -> c) 
        |> Seq.max 
        |> (+) 1

    let maxRow =
        map 
        |> Map.toSeq
        |> Seq.map (fun ((r, _), _) -> r)
        |> Seq.max
        |> (+) 1

    let dumpLine r =
        [0..maxCol]
        |> Seq.iter (fun c ->
            map
            |> Map.tryFind (r, c)
            |> Option.defaultValue Wall
            |> toChar
            |> printf "%c")

        printfn ""

    [0..maxRow]
    |> Seq.iter dumpLine

printfn "%A" input

dumpMap input

