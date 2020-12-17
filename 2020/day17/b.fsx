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
        | '#' -> [(row, col, 0, 0)]
        | _ -> [])

let parseGrid input =
    (enumerate input)
    |> Seq.collect (fun (row, line) -> parseLine row line)
    |> Set.ofSeq

// from https://stackoverflow.com/questions/9213761/cartesian-product-two-lists
let cartesian xs ys =
    xs |> Seq.collect (fun x -> ys |> Seq.map (fun y -> x, y))

let cartesian4 xs ys zs ws =
    xs
    |> cartesian ys
    |> cartesian zs
    |> cartesian ws
    |> Seq.map (fun (d, (c, (b, a))) -> (a, b, c, d))
    |> Seq.toList

let neighbors (row, col, layer, phase) =
    cartesian4 [row - 1; row; row + 1] [col - 1; col; col + 1] [layer - 1; layer; layer + 1] [phase - 1; phase; phase + 1]
    |> Set.ofSeq
    |> Set.remove (row, col, layer, phase)

let liveNeighbors grid cell =
    cell
    |> neighbors
    |> Set.filter (fun n -> Set.contains n grid)
    |> Set.count

let step grid =
    let activeCells =
        grid
        |> Set.toList
        |> List.map neighbors
        |> Set.unionMany

    let isAlive oldGrid cell =
        let n = liveNeighbors oldGrid cell
        if (Set.contains cell oldGrid) && n = 2 then true else (n = 3)

    let newGrid =
        activeCells
        |> Set.filter (isAlive grid)

    newGrid

(parseGrid input)
        |> step
        |> step
        |> step
        |> step
        |> step
        |> step
        |> Set.count
        |> printfn "%A"


     
