// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let input =
    readLines "input.txt"
    |> Seq.toList
    |> List.collect (function
        | Regex "^x=(-?\d+), y=(-?\d+)..(-?\d+)$" [xt; y1t; y2t] ->
            [(int y1t)..(int y2t)] |> List.map (fun y -> (int xt, y))
        | Regex "^y=(-?\d+), x=(-?\d+)..(-?\d+)$" [yt; x1t; x2t] ->
            [(int x1t)..(int x2t)] |> List.map (fun x -> (x, int yt))
        | s ->
            printfn "Invalid scan value [%A]" s
            [])

type State = Sand | Clay | FlowingWater | SettledWater

let minY = input |> Seq.map (fun (_, y) -> y) |> Seq.min
let maxY = input |> Seq.map (fun (_, y) -> y) |> Seq.max

printfn "%A %A" minY maxY

let initialGrid input =
    input
    |> List.map (fun x -> (x, Clay))
    |> Map.ofList

printfn "%A" (initialGrid input)

let rec fill grid (x, y) =
    let getCell grid (x, y) = Map.tryFind (x, y) grid |> Option.defaultValue Sand

    let below = (x, y + 1)
    let left = (x - 1, y)
    let right = (x + 1, y)

    let rec findWall grid (x', y') offset =
        match getCell grid (x', y') with
        | Sand -> 
            None
        | Clay ->
            Some (x', y')
        | SettledWater ->
            None
        | FlowingWater ->
            findWall grid (x' + offset, y') offset

    let hasWalls grid (x, y) =
        let hasWall (x, y) offset = match findWall grid (x, y) offset with Some _ -> true | None -> false

        hasWall (x, y) 1 && hasWall (x, y) -1

    let settle grid (x, y) =
        match getCell grid (x, y) with
        | FlowingWater ->
            let leftWall = findWall grid (x, y) -1
            let rightWall = findWall grid (x, y) 1

            match (leftWall, rightWall) with
            | (Some (l, _), Some (r, _)) ->
                [(l + 1)..(r - 1)]
                |> List.map (fun x -> (x, y))
                |> List.fold (fun acc elem -> Map.add elem SettledWater acc) grid
            | _ -> failwith "impossible state, settling without walls"
        | _ -> failwith "impossible state, settling something other than flowing water"

    if y >= maxY then
        grid
    else
        let grid'1 =
            if (getCell grid below) = Sand then 
                fill (Map.add below FlowingWater grid) below
            else
                grid

        let grid'2 =
            if ((getCell grid'1 below) = Clay || (getCell grid'1 below) = SettledWater) && (getCell grid'1 left) = Sand then
                fill (Map.add left FlowingWater grid'1) left
            else
                grid'1

        let grid'3 =
            if ((getCell grid'2 below) = Clay || (getCell grid'2 below) = SettledWater) && (getCell grid'2 right) = Sand then
                fill (Map.add right FlowingWater grid'2) right
            else
                grid'2

        let grid'4 =
            if hasWalls grid'3 (x, y) then
                settle grid'3 (x, y)
            else
                grid'3

        grid'4

let result = fill (initialGrid input) (500, 0)

printfn "%A" result

result
|> Map.toList
|> List.filter (fun ((x, y), t) -> t = SettledWater)
|> List.filter (fun ((_, y), _) -> y >= minY && y <= maxY)
|> List.length
|> printfn "%A"
