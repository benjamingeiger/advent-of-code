// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic
open System.Text.RegularExpressions

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let enumerate s = s |> Seq.zip (Seq.initInfinite (fun n -> n))

let parseLine row line =
    enumerate line
    |> Seq.collect (fun (col, c) ->
        match c with
        | '#' -> [(row, col, 0)]
        | _ -> [])

let parseGrid input =
    (enumerate input)
    |> Seq.collect (fun (row, line) -> parseLine row line)
    |> Seq.toList

// from https://stackoverflow.com/questions/9213761/cartesian-product-two-lists
let cartesian xs ys =
    xs |> Seq.collect (fun x -> ys |> Seq.map (fun y -> x, y))

let cartesian3 xs ys zs =
    xs
    |> cartesian ys
    |> cartesian zs
    |> Seq.map (fun (c, (b, a)) -> (a, b, c))
    |> Seq.toList

let neighbors (row, col, layer) =
    cartesian3 [row - 1; row; row + 1] [col - 1; col; col + 1] [layer - 1; layer; layer + 1]
    |> Seq.filter ((<>) (row, col, layer))
    |> Seq.toList

let liveNeighbors grid cell =
    cell
    |> neighbors
    |> List.filter (fun n -> List.contains n grid)
    |> List.length

let step grid =
    let activeCells =
        grid
        |> List.collect (fun cell -> cell :: neighbors cell)
        |> List.distinct

    let isAlive oldGrid cell =
        let n = liveNeighbors oldGrid cell
        if (List.contains cell oldGrid) && n = 2 then true else (n = 3)

    let newGrid =
        activeCells
        |> List.filter (isAlive grid)

    newGrid

(parseGrid input)
        |> step
        |> step
        |> step
        |> step
        |> step
        |> step
        |> List.length
        |> printfn "%A"


     
