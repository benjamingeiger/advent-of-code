// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

open System.Text.RegularExpressions

let rawInput = readLines "input.txt" |> List.ofSeq

type Cell =
    | Empty
    | Wall

type Facing =
    | Right
    | Down
    | Left
    | Up

type Step =
    | Forward of int
    | TurnLeft
    | TurnRight

let parseInput rawInput =
    let segments =
        rawInput
        |> splitSeq ((=) "")

    let mapText = segments |> Seq.item 0
    let (routeText : string) = segments |> Seq.item 1 |> Seq.head

    let map =
        mapText
        |> indexMapCharacters (fun r c x -> match x with | '.' -> Some ((r, c), Empty) | '#' -> Some ((r, c), Wall) | _ -> None)
        |> Seq.choose id
        |> Map.ofSeq

    let route =
        Regex.Split(routeText, "(L|R)")
        |> Seq.map (fun cs ->
            match System.Int32.TryParse cs with
            | (true, value) -> Forward value
            | (false, _) ->
                if cs = "L" then TurnLeft
                elif cs = "R" then TurnRight
                else failwithf "invalid route step %A" cs)
        |> List.ofSeq

    (map, route)

let rec evaluate map ((r, c), facing) instruction =
    printfn "%A %A %A" (r + 1, c + 1) facing instruction
    match instruction with
    | TurnLeft -> ((r, c), match facing with | Right -> Up | Up -> Left | Left -> Down | Down -> Right)
    | TurnRight -> ((r, c), match facing with | Right -> Down | Down -> Left | Left -> Up | Up -> Right)
    | Forward 0 -> ((r, c), facing)
    | Forward n ->
        match facing with
        | Right ->
            let nextPos = (r, c + 1)
            match map |> Map.tryFind nextPos with
            | Some Empty -> evaluate map (nextPos, facing) (Forward (n - 1))
            | Some Wall -> ((r, c), facing)
            | None ->
                // we fell off the edge of the map
                let wrapped = map |> Map.keys |> Seq.filter (fun (r', _) -> r' = r) |> Seq.minBy snd
                match map |> Map.tryFind wrapped with
                | Some Empty -> evaluate map (wrapped, facing) (Forward (n - 1))
                | Some Wall -> ((r, c), facing)
                | None -> failwithf "somehow we got stuck: %A" ((r, c), facing)
        | Down ->
            let nextPos = (r + 1, c)
            match map |> Map.tryFind nextPos with
            | Some Empty -> evaluate map (nextPos, facing) (Forward (n - 1))
            | Some Wall -> ((r, c), facing)
            | None ->
                // we fell off the edge of the map
                let wrapped = map |> Map.keys |> Seq.filter (fun (_, c') -> c' = c) |> Seq.minBy fst
                match map |> Map.tryFind wrapped with
                | Some Empty -> evaluate map (wrapped, facing) (Forward (n - 1))
                | Some Wall -> ((r, c), facing)
                | None -> failwithf "somehow we got stuck: %A" ((r, c), facing)
        | Left ->
            let nextPos = (r, c - 1)
            match map |> Map.tryFind nextPos with
            | Some Empty -> evaluate map (nextPos, facing) (Forward (n - 1))
            | Some Wall -> ((r, c), facing)
            | None ->
                // we fell off the edge of the map
                let wrapped = map |> Map.keys |> Seq.filter (fun (r', _) -> r' = r) |> Seq.maxBy snd
                match map |> Map.tryFind wrapped with
                | Some Empty -> evaluate map (wrapped, facing) (Forward (n - 1))
                | Some Wall -> ((r, c), facing)
                | None -> failwithf "somehow we got stuck: %A" ((r, c), facing)
        | Up ->
            let nextPos = (r - 1, c)
            match map |> Map.tryFind nextPos with
            | Some Empty -> evaluate map (nextPos, facing) (Forward (n - 1))
            | Some Wall -> ((r, c), facing)
            | None ->
                // we fell off the edge of the map
                let wrapped = map |> Map.keys |> Seq.filter (fun (_, c') -> c' = c) |> Seq.maxBy fst
                match map |> Map.tryFind wrapped with
                | Some Empty -> evaluate map (wrapped, facing) (Forward (n - 1))
                | Some Wall -> ((r, c), facing)
                | None -> failwithf "somehow we got stuck: %A" ((r, c), facing)

let run input =
    let (map, route) = input

    let startPos =
        map
        |> Map.filter (fun (r, _) v -> r = 0 && v = Empty)
        |> Map.keys
        |> Seq.minBy snd

    let ((r, c), facing) = List.fold (evaluate map) (startPos, Right) route

    (r + 1) * 1000 + (c + 1) * 4 + (match facing with | Right -> 0 | Down -> 1 | Left -> 2 | Up -> 3)

doProcess parseInput run rawInput
