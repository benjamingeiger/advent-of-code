// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> List.ofSeq

type Cucumber = East | South

let parseInput input =
    input
    |> indexCharacters (function
        | '>' -> Some East
        | 'v' -> Some South
        | _ -> None)
    |> Seq.choose (fun ((x, y), c) -> match c with | Some c' -> Some ((x, y), c') | None -> None)
    |> Map.ofSeq

let countSize input =
    let nr = 
        input
        |> Map.toSeq
        |> Seq.map (fst >> fst)
        |> Seq.max
        |> ((+) 1)

    let nc = 
        input
        |> Map.toSeq
        |> Seq.map (fst >> snd)
        |> Seq.max
        |> ((+) 1)

    (nr, nc)

let dump nr nc map =
    [0 .. (nr - 1)]
    |> List.map (fun r ->
        [0 .. (nc - 1)]
        |> List.map (fun c ->
            match Map.tryFind (r, c) map with
            | Some x when x = East -> ">"
            | Some x when x = South -> "v"
            | None -> ".")
        |> String.concat "")
    |> String.concat "\n"

let moveCucumbers nr nc map =
    let eastStep =
        map
        |> Map.toSeq
        |> Seq.filter (fun (_, v) -> v = East)
        |> Seq.filter (fun ((r, c), _) -> map |> Map.tryFind (r, (c + 1) % nc) = None)
        |> Seq.fold (fun acc ((r, c), _) -> acc |> Map.remove (r, c) |> Map.add (r, (c + 1) % nc) East) map

    let southStep =
        eastStep
        |> Map.toSeq
        |> Seq.filter (fun (_, v) -> v = South)
        |> Seq.filter (fun ((r, c), _) -> eastStep |> Map.tryFind ((r + 1) % nr, c) = None)
        |> Seq.fold (fun acc ((r, c), _) -> acc |> Map.remove (r, c) |> Map.add ((r + 1) % nr, c) South) eastStep

    (*printfn "%s" (dump nr nc southStep)*)
    (*printfn ""*)

    southStep

let repeat f input =
    let rec go n input =
        let next = f input
        if input = next then n + 1 else go (n + 1) next

    go 0 input

let result =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let parsed = parseInput input

    let numRows, numCols = countSize parsed

    (*printfn "%s" (dump numRows numCols parsed)*)
    (*printfn ""*)

    let returnValue = repeat (moveCucumbers numRows numCols) parsed

    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds

    returnValue

printfn "%A" result
