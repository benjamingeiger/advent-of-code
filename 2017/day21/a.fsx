// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type Pattern = Pattern of bool list list list * bool list list

let splitPattern (cs : string) =
    cs.Split("/")
    |> Array.toList
    |> List.map (fun s -> s |> Seq.map (fun c -> if c = '#' then true else false) |> List.ofSeq)

let rotateAndFlipPattern (cs : bool list list) =
    let v = List.rev
    let h = List.map List.rev
    let vh = v >> h
    let t = List.transpose
    let tv = t >> v
    let th = t >> h
    let tvh = t >> v >> h

    [id; v; h; vh; t; tv; th; tvh]
    |> List.map (fun f -> f cs)
    |> List.distinct

let readLines filePath = System.IO.File.ReadLines filePath
let input =
    readLines "input.txt"
    |> Seq.choose (function
        | Regex "^(\S+) => (\S+)$" [before; after] ->
            Some (Pattern (rotateAndFlipPattern (splitPattern before), splitPattern after))
        | s ->
            printfn "Invalid pattern [%A]" s
            None)
    |> Seq.toList

let stackHorizontally x1 x2 = List.zip x1 x2 |> List.map (fun (x1', x2') -> x1' @ x2')

let stackVertically x1 x2 = x1 @ x2

let stackHorizontallyMany = function
    | [] -> []
    | [x] -> x
    | x::xs -> List.fold stackHorizontally x xs

let stackVerticallyMany = function
    | [] -> []
    | [x] -> x
    | x::xs -> List.fold stackVertically x xs

let splitVertically size xs = List.chunkBySize size xs

let splitHorizontally size xs =
    xs
    |> List.map (List.chunkBySize size)
    |> List.transpose;

let splitUp size xs =
    xs
    |> splitVertically size
    |> List.map (splitHorizontally size)

let tick patterns grid =
    let chunkSize = if (grid |> List.length) % 2 = 0 then 2 else 3

    let matchPattern chunk =
        patterns
        |> List.choose (fun (Pattern (before, after)) -> if List.contains chunk before then Some after else None)
        |> List.exactlyOne

    let chunks =
        grid                                                // bool list list
        |> splitUp chunkSize                                // bool list list list list
        |> List.map (List.map matchPattern)
        |> List.map stackHorizontallyMany
        |> stackVerticallyMany

    chunks

let initialGrid = [[false; true; false]; [false; false; true]; [true; true; true]]

(*printfn "%A" initialGrid*)
(*printfn "%A" (tick input initialGrid)*)
(*printfn "%A" (tick input (tick input initialGrid))*)

let countCells grid =
    grid
    |> List.map (List.sumBy (fun c -> if c then 1 else 0))
    |> List.sum

let wrappedTick patterns grid =
    let output = tick patterns grid
    Some (countCells output, output)

let gridSequence =
    Seq.unfold (wrappedTick input) initialGrid

let result = gridSequence |> Seq.take 5 |> Seq.last

// part 2
// let result = gridSequence |> Seq.take 18 |> Seq.last

printfn "%A" result
