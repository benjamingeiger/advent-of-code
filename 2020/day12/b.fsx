// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic 

let memoize (f : 'a -> 'b) =
    let dict = new Dictionary<'a, 'b>()

    let memoizedFunc (input : 'a) =
        match dict.TryGetValue(input) with
        | true, x -> x
        | false, _ ->
            let answer = f input
            dict.Add(input, answer)
            answer

    memoizedFunc

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

type Command = North of int | South of int | East of int | West of int | Left of int | Right of int | Forward of int

let parse (line : string) =
    match line.[0] with
    | 'N' -> North (int line.[1..])
    | 'E' -> East (int line.[1..])
    | 'S' -> South (int line.[1..])
    | 'W' -> West (int line.[1..])
    | 'L' -> Left (int line.[1..])
    | 'R' -> Right (int line.[1..])
    | 'F' -> Forward (int line.[1..])
    | _ -> failwith (sprintf "Invalid line %A" line)

let commands = input |> Seq.map parse

let rec step (waypointNS, waypointEW, latitude, longitude) command =
    match command with
    | North n -> (waypointNS + n, waypointEW, latitude, longitude)
    | East n -> (waypointNS, waypointEW + n, latitude, longitude)
    | South n -> (waypointNS - n, waypointEW, latitude, longitude)
    | West n -> (waypointNS, waypointEW - n, latitude, longitude)
    | Left n ->
        match n with
        | 0 -> (waypointNS, waypointEW, latitude, longitude)
        | 90 -> (waypointEW, -1 * waypointNS, latitude, longitude)
        | 180 -> (-1 * waypointNS, -1 * waypointEW, latitude, longitude)
        | 270 -> (-1 * waypointEW, waypointNS, latitude, longitude)
        | _ -> failwith (sprintf "invalid left turn %A" n)
    | Right n ->
        match n with
        | 0 -> (waypointNS, waypointEW, latitude, longitude)
        | 90 -> (-1 * waypointEW, waypointNS, latitude, longitude)
        | 180 -> (-1 * waypointNS, -1 * waypointEW, latitude, longitude)
        | 270 -> (waypointEW, -1 * waypointNS, latitude, longitude)
        | _ -> failwith (sprintf "invalid right turn %A" n)
    | Forward n -> (waypointNS, waypointEW, latitude + (n * waypointNS), longitude + (n * waypointEW))

let (waypointNS, waypointEW, latitude, longitude) = Seq.fold step (1, 10, 0, 0) commands
printfn "%A" ((abs latitude) + (abs longitude))
