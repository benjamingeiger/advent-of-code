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

let takeStep (dir, visited) step =
    let rec markNorth (initLat, initLong) n =
        if n <= 0 then [(initLat, initLong)] else (markNorth ((initLat + 1), initLong) (n - 1)) @ [(initLat, initLong)]
    let rec markEast (initLat, initLong) n =
        if n <= 0 then [(initLat, initLong)] else (markEast (initLat, (initLong + 1)) (n - 1)) @ [(initLat, initLong)]
    let rec markSouth (initLat, initLong) n =
        if n <= 0 then [(initLat, initLong)] else (markSouth ((initLat - 1), initLong) (n - 1)) @ [(initLat, initLong)]
    let rec markWest (initLat, initLong) n =
        if n <= 0 then [(initLat, initLong)] else (markWest (initLat, (initLong - 1)) (n - 1)) @ [(initLat, initLong)]

    match (dir, step) with
    | North, (Left n) -> (West, ((markWest (List.head visited) n) @ (List.tail visited)))
    | North, (Right n) -> (East, ((markEast (List.head visited) n) @ (List.tail visited)))
    | East, (Left n) -> (North, ((markNorth (List.head visited) n) @ (List.tail visited)))
    | East, (Right n) -> (South, ((markSouth (List.head visited) n) @ (List.tail visited)))
    | South, (Left n) -> (East, ((markEast (List.head visited) n) @ (List.tail visited)))
    | South, (Right n) -> (West, ((markWest (List.head visited) n) @ (List.tail visited)))
    | West, (Left n) -> (South, ((markSouth (List.head visited) n) @ (List.tail visited)))
    | West, (Right n) -> (North, ((markNorth (List.head visited) n) @ (List.tail visited)))


let finalDir, visited = Seq.fold takeStep (North, [(0, 0)]) steps

let rec dupes = function
    | [] -> []
    | (h :: t) when (List.exists ((=) h) t) -> h :: dupes t
    | (h :: t) -> dupes t

visited
    |> dupes
    |> Seq.rev
    |> Seq.head
    |> fun (x, y) -> ((abs x) + (abs y))
    |> printfn "%A"
