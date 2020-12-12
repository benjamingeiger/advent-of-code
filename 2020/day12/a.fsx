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

let rec step (heading, latitude, longitude) command =
    match command with
    | North n -> (heading, latitude + n, longitude)
    | East n -> (heading, latitude, longitude + n)
    | South n -> (heading, latitude - n, longitude)
    | West n -> (heading, latitude, longitude - n)
    | Left n -> ((heading + 360 - n) % 360, latitude, longitude)
    | Right n -> ((heading + n) % 360, latitude, longitude)
    | Forward n ->
        match heading with
        | 0 -> step (heading, latitude, longitude) (North n)
        | 90 -> step (heading, latitude, longitude) (East n)
        | 180 -> step (heading, latitude, longitude) (South n)
        | 270 -> step (heading, latitude, longitude) (West n)
        | _ -> failwith (sprintf "impossible heading %A" heading)

let (heading, latitude, longitude) = Seq.fold step (90, 0, 0) commands
printfn "%A" ((abs latitude) + (abs longitude))
printfn "%A" (heading, latitude, longitude)
