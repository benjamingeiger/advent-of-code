// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> List.ofSeq

let parseInput input =
    let pointsText, foldsText =
        readLines "input.txt"
        |> splitSeq ((=) "")
        |> fun s -> Seq.item 0 s, Seq.item 1 s

    let points =
        pointsText
        |> Seq.choose (function
            | Regex "(\d+),(\d+)" [x; y] -> Some (int x, int y)
            | x ->
                printfn "invalid point %A" x
                None)
        |> Set.ofSeq

    let folds =
        foldsText
        |> Seq.choose (function
            | Regex "fold along (.)=(\d+)" [axis; line] ->
                Some (axis, int line)
            | x ->
                printfn "invalid fold instruction %A" x
                None)
        |> List.ofSeq

    points, folds

let doFold points (axis, line) =
    match axis with
    | "x" -> points |> Set.map (fun (x, y) -> ((if x < line then x else (2 * line - x)), y))
    | "y" -> points |> Set.map (fun (x, y) -> (x, (if y < line then y else (2 * line - y))))
    | z ->
        printfn "Invalid fold axis %A" z
        points

let dumpImage points =
    let numCols = points |> Set.toSeq |> Seq.maxBy fst |> fst
    let numRows = points |> Set.toSeq |> Seq.maxBy snd |> snd

    [0..numRows]
    |> List.map (fun row ->
        [0..numCols]
        |> List.map (fun col -> if Set.contains (col, row) points then "█" else " ")
        |> String.concat "")
    |> String.concat "\n"

let result =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let (points, folds) = parseInput input

    let returnValue =
        folds
        |> List.fold doFold points
        |> dumpImage

    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds

    returnValue

printfn "%s" result
