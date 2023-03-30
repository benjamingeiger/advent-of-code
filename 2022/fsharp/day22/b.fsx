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
        |> indexMapCharacters (fun r c x -> match x with | '.' -> Some ((r + 1, c + 1), Empty) | '#' -> Some ((r + 1, c + 1), Wall) | _ -> None)
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

// this is specific to my input, so be it
let wrap ((r, c), facing) =
    printfn "wrapping from %A" ((r, c), facing)
    match facing with
    // note: we start on top
    | Right ->
        if r <= 50 then // walking from right to bottom
            ((150 - r + 1, 100), Left)
        elif r <= 100 then // walking from front to right
            ((50, r + 50), Up)
        elif r <= 150 then // walking from bottom to right
            ((150 - r + 1, 150), Left)
        else // walking from back to bottom
            ((150, r - 100), Up)
    | Down ->
        if c <= 50 then // back to right
            ((1, c + 100), Down)
        elif c <= 100 then // bottom to back
            ((c + 100, 50), Left)
        else // right to front
            ((c - 50, 100), Left)
    | Left ->
        if r <= 50 then // top to Left
            ((150 - r + 1, 1), Right)
        elif r <= 100 then // front to Left
            ((101, r - 50), Down)
        elif r <= 150 then // left to top
            ((150 - r + 1, 51), Right)
        else // back to top
            ((1, r - 100), Down)
    | Up ->
        if c <= 50 then // left to front
            ((c + 50, 51), Right)
        elif c <= 100 then // top to back
            ((c + 100, 1), Right)
        else // right to back
            ((200, c - 100), Up)


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
                let (wrapped, newFacing) = wrap ((r, c), facing)
                match map |> Map.tryFind wrapped with
                | Some Empty -> evaluate map (wrapped, newFacing) (Forward (n - 1))
                | Some Wall -> ((r, c), facing)
                | None -> failwithf "somehow we got stuck: %A" ((r, c), facing)
        | Down ->
            let nextPos = (r + 1, c)
            match map |> Map.tryFind nextPos with
            | Some Empty -> evaluate map (nextPos, facing) (Forward (n - 1))
            | Some Wall -> ((r, c), facing)
            | None ->
                // we fell off the edge of the map
                let (wrapped, newFacing) = wrap ((r, c), facing)
                match map |> Map.tryFind wrapped with
                | Some Empty -> evaluate map (wrapped, newFacing) (Forward (n - 1))
                | Some Wall -> ((r, c), facing)
                | None -> failwithf "somehow we got stuck: %A" ((r, c), facing)
        | Left ->
            let nextPos = (r, c - 1)
            match map |> Map.tryFind nextPos with
            | Some Empty -> evaluate map (nextPos, facing) (Forward (n - 1))
            | Some Wall -> ((r, c), facing)
            | None ->
                // we fell off the edge of the map
                let (wrapped, newFacing) = wrap ((r, c), facing)
                match map |> Map.tryFind wrapped with
                | Some Empty -> evaluate map (wrapped, newFacing) (Forward (n - 1))
                | Some Wall -> ((r, c), facing)
                | None -> failwithf "somehow we got stuck: %A" ((r, c), facing)
        | Up ->
            let nextPos = (r - 1, c)
            match map |> Map.tryFind nextPos with
            | Some Empty -> evaluate map (nextPos, facing) (Forward (n - 1))
            | Some Wall -> ((r, c), facing)
            | None ->
                // we fell off the edge of the map
                let (wrapped, newFacing) = wrap ((r, c), facing)
                match map |> Map.tryFind wrapped with
                | Some Empty -> evaluate map (wrapped, newFacing) (Forward (n - 1))
                | Some Wall -> ((r, c), facing)
                | None -> failwithf "somehow we got stuck: %A" ((r, c), facing)

let run input =
    let (map, route) = input

    let startPos =
        map
        |> Map.filter (fun (r, _) v -> r = 1 && v = Empty)
        |> Map.keys
        |> Seq.minBy snd

    let ((r, c), facing) = List.fold (evaluate map) (startPos, Right) route

    r * 1000 + c * 4 + (match facing with | Right -> 0 | Down -> 1 | Left -> 2 | Up -> 3)

doProcess parseInput run rawInput
