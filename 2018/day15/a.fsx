// vim: set et ts=4 sw=4 list :

open System

type Cell = Wall | Empty | Goblin of int | Elf of int

let initHitPoints = 200
let attackPower = 3

let readLines filePath = System.IO.File.ReadLines filePath
let input =
    readLines "input.txt"
    |> Seq.map Seq.indexed
    |> Seq.indexed
    |> Seq.collect (fun (r, cs) -> cs |> Seq.map (fun (c, v) -> ((r, c), v)))
    |> Seq.choose (function
        | ((r, c), '#') -> None
        | ((r, c), '.') -> Some ((r, c), Empty)
        | ((r, c), 'G') -> Some ((r, c), Goblin initHitPoints)
        | ((r, c), 'E') -> Some ((r, c), Elf initHitPoints)
        | x ->
            eprintfn "Unknown input [%A]" x
            None)
    |> Map.ofSeq

let dumpMap map =
    let toChar = function
        | Empty -> '.'
        | Goblin _ -> 'G'
        | Elf _ -> 'E'
        | Wall -> ' '

    let maxCol =
        map
        |> Map.toSeq
        |> Seq.map (fun ((_, c), _) -> c) 
        |> Seq.max 
        |> (+) 1

    let maxRow =
        map 
        |> Map.toSeq
        |> Seq.map (fun ((r, _), _) -> r)
        |> Seq.max
        |> (+) 1

    let dumpLine r =
        [0..maxCol]
        |> Seq.iter (fun c ->
            map
            |> Map.tryFind (r, c)
            |> Option.defaultValue Wall
            |> toChar
            |> printf "%c")

        printfn ""

    [0..maxRow]
    |> Seq.iter dumpLine

printfn "%A" input

dumpMap input

module Seq =
    let tryMin xs =
        if Seq.isEmpty xs then None else Seq.min xs |> Some

let doTurn map (r, c) =
    let manhattan (r', c') = abs (r' - r) + abs (c' - c)

    let getCell (r', c') = Map.find (r', c') map

    let isEnemy u v =
        match u, v with
        | Goblin _, Elf _ -> true
        | Elf _, Goblin _ -> true
        | _, _ -> false

    let targets u = 
        map
        |> Map.filter (fun _ v -> isEnemy u v)
        |> Map.toSeq

    let adjacent (r', c') = [(r' - 1, c'); (r', c' - 1); (r', c' + 1); (r' + 1, c')]

    let attack (r', c') =
        match getCell (r', c') with
        | Some (Elf hp) ->
            map
            |> Map.add (r', c') (
                if hp <= attackPower then
                    Elf (hp - attackPower)
                else
                    Empty)
        | Some (Goblin hp) ->
            map
            |> Map.add (r', c') (
                if hp <= attackPower then
                    Goblin (hp - attackPower)
                else
                    Empty)
        | _ -> map

    let move targets =
        // todo: work on this next, need to implement bfs.
        // maybe run bfs from any open spaces near the current unit
        // and take the one with the shortest distance to the target
        // (taking the first (top, left, right, bottom) if there are ties)
        // so we don't have to worry about multipathing?
        map

    // need to add a wrapper around this to check for end of round, since
    // once there are no more opponents all fighting stops and the round ends
    //
    // actually, wait? can we wait until the end of the round to determine
    // whether combat is over? the question is, if (w.l.o.g.) the last goblin
    // dies on the last turn of the round, does it count as a complete round?
    match Map.tryFind (r, c) map with
    | None | Some Wall | Some Empty -> map
    | Some u ->
        let targets =
            map
            |> Map.filter (fun _ v -> isEnemy u v)
            |> Map.toSeq

        adjacent (r, c)
        |> Seq.tryFind (fun (r', c') -> Seq.contains (r', c') targets)
        |> function
            | Some (r', c') -> attack (r', c')
            | None ->
                move targets
                // need to change this logic. maybe always "move" then attack if adjacent,
                // but don't actually move if already adjacent?

let doRound map =

    let unitPositions =
        map
        |> Map.filter (fun _ v ->
            match v with
            | Wall | Empty -> false
            | Elf _ | Goblin _ -> true)
        |> Map.toList
        |> List.map (fun ((r, c), _) -> (r, c))

    Seq.fold doTurn map unitPositions

dumpMap (doRound input)
