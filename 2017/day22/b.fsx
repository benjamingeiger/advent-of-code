// vim: set et ts=4 sw=4 list :

open System

type Direction = Up | Right | Down | Left

let turnRight = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let turnAround = turnRight >> turnRight

let turnLeft = turnRight >> turnRight >> turnRight

let stepForward (r, c) = function
    | Up -> (r - 1, c)
    | Right -> (r, c + 1)
    | Down -> (r + 1, c)
    | Left -> (r, c - 1)

type State = Clean | Weakened | Infected | Flagged

let stepState = function
    | Clean -> Weakened
    | Weakened -> Infected
    | Infected -> Flagged
    | Flagged -> Clean

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let rows = Seq.length input
let cols = Seq.length (Seq.head input)

let isInfected c = (c = '#')
let parseLine l =
    l
    |> Seq.map (fun c -> if isInfected c then Infected else Clean)
    |> Seq.indexed

let parseGrid ls =
    ls
    |> Seq.indexed
    |> Seq.map (fun (r, l) -> (l |> parseLine |> Seq.map (fun (c, x) -> ((r, c), x))))
    |> Seq.concat

let initialGrid = input |> parseGrid |> Map.ofSeq

type Carrier = {
    position: int * int
    direction: Direction
}

let initialCarrier = { position = (rows / 2, cols / 2); direction = Up }

let getInfection (r, c) grid = match Map.tryFind (r, c) grid with | Some state -> state | none -> Clean

let burst (carrier, grid) =
    let move turn =
        let newDirection = turn carrier.direction
        let newPosition = stepForward carrier.position newDirection

        { carrier with direction = newDirection; position = newPosition }

    let update state = Map.add carrier.position (stepState state) grid

    (*printfn "%A %A %A" (getInfection carrier.position grid) carrier grid*)

    match getInfection carrier.position grid with
    | Clean -> Some (0, (move turnLeft, update Clean))
    | Weakened -> Some (1, (move id, update Weakened))
    | Infected -> Some (0, (move turnRight, update Infected))
    | Flagged -> Some (0, (move turnAround, update Flagged))

let output = Seq.unfold burst (initialCarrier, initialGrid) |> Seq.take 10000000 |> Seq.sum

printfn "%A" output


