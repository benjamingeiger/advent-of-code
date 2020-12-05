// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

type Direction = North | East | South | West
type Turn = Left of int | Right of int

let parseStep (s : string) =
    let distance = s.[1..] |> int
    match s.[0] with
    | 'L' -> Left distance
    | 'R' -> Right distance
    | _ -> failwith "Invalid direction"

let steps =
    (Seq.exactlyOne input).Split ", "
    |> Seq.map parseStep

let takeStep (dir, lat, long) step =
    match (dir, step) with
    | North, (Left n) -> (West, lat, long - n)
    | North, (Right n) -> (East, lat, long + n)
    | East, (Left n) -> (North, lat + n, long)
    | East, (Right n) -> (South, lat - n, long)
    | South, (Left n) -> (East, lat, long + n)
    | South, (Right n) -> (West, lat, long - n)
    | West, (Left n) -> (South, lat - n, long)
    | West, (Right n) -> (North, lat + n, long)

let (_, finalLat, finalLong) = Seq.fold takeStep (North, 0, 0) steps
printfn "%A" ((abs finalLat) + (abs finalLong))



