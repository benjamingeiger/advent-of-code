// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

// position, velocity, acceleration
type Dimension = Dimension of int * int * int

// x, y, z
type Point = Point of Dimension * Dimension * Dimension

let integerRegex = "(-?\d+)"
let positionRegex = String.concat "" ["p=<"; integerRegex; ","; integerRegex; ","; integerRegex; ">"]
let velocityRegex = String.concat "" ["v=<"; integerRegex; ","; integerRegex; ","; integerRegex; ">"]
let accelerationRegex = String.concat "" ["a=<"; integerRegex; ","; integerRegex; ","; integerRegex; ">"]

let lineRegex = String.concat ", " [positionRegex; velocityRegex; accelerationRegex]

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.choose (function
    | Regex lineRegex [sx; sy; sz; vx; vy; vz; ax; ay; az] ->
        Some (
            Point (
                Dimension (int sx, int vx, int ax),
                Dimension (int sy, int vy, int ay),
                Dimension (int sz, int vz, int az)))
    | s ->
        printfn "invalid point [%A]" s
        None) |> List.ofSeq

let updatePoint (Point (x, y, z)) =
    let updateDimension (Dimension (s, v, a)) = Dimension (s + (v + a), v + a, a)
    Point (updateDimension x, updateDimension y, updateDimension z)

let distance (Point (Dimension (sx, _, _), Dimension (sy, _, _), Dimension (sz, _, _))) =
    abs sx + abs sy + abs sz

let velocity p = distance (updatePoint p) - distance p

let acceleration p = velocity (updatePoint p) - velocity p

let simulate ps =
    let update = List.map updatePoint

    let rec tick ps =
        let next = update ps

        if List.exists (fun p -> (acceleration p) < 0) next
        then tick next
        else
            let closest = List.minBy distance next
            let slowest = List.minBy velocity next

            if closest = slowest
            then List.findIndex ((=) closest) next
            else tick next

    tick ps

let result = simulate input

printfn "%A" result
