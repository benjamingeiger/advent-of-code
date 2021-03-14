// vim: set et ts=4 sw=4 list :

open System

type Direction = Up | Right | Down | Left

let turnRight = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let turnLeft = function
    | Up -> Left
    | Left -> Down
    | Down -> Right
    | Right -> Up

let forward (r, c) = function
    | Up -> (r - 1, c)
    | Right -> (r, c + 1)
    | Down -> (r + 1, c)
    | Left -> (r, c - 1)

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let rows = Seq.length input
let cols = Seq.length (Seq.head input)

let isInfected c = (c = '#')
let parseLine l =
    l
    |> Seq.indexed
    |> Seq.choose (fun (i, c) -> if isInfected c then Some i else None)

let parseGrid ls =
    ls
    |> Seq.indexed
    |> Seq.map (fun (i, l) -> (l |> parseLine |> Seq.map (fun x -> (i, x))))
    |> Seq.concat

let initialInfections = input |> parseGrid |> Set.ofSeq

type Carrier = {
    position: int * int
    direction: Direction
}

let initialCarrier = { position = (rows / 2, cols / 2); direction = Up }

let burst (carrier, infections) =
    let isInfected = Set.contains carrier.position infections
    let newDirection = if isInfected then turnRight carrier.direction else turnLeft carrier.direction
    let newInfections = if isInfected then Set.remove carrier.position infections else Set.add carrier.position infections
    let newPosition = forward carrier.position newDirection

    (*printfn "%A %A %A %A %A" carrier.position isInfected newPosition newDirection newInfections*)

    let newCarrier =
        { carrier with
            direction = newDirection
            position = newPosition }

    Some ((if isInfected then 0 else 1), (newCarrier, newInfections))

let output = Seq.unfold burst (initialCarrier, initialInfections) |> Seq.take 10000 |> Seq.sum

printfn "%A" output


