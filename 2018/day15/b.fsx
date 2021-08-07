// vim: set et ts=4 sw=4 list :

open System

type Cell = Wall | Empty | Goblin of int | Elf of int
type Battlefield = Map<int * int, Cell>
type CombatStatus = Fighting | CombatEnded | MissionFailed
type State = Battlefield * CombatStatus

let initHitPoints = 200
let goblinAttackPower = 3

let readLines filePath = System.IO.File.ReadLines filePath
let input : Battlefield =
    readLines "input.txt"
    |> Seq.map Seq.indexed
    |> Seq.indexed
    |> Seq.collect (fun (r, cs) -> cs |> Seq.map (fun (c, v) -> ((r, c), v)))
    |> Seq.choose (function
        | ((r, c), '#') -> Some ((r, c), Wall)
        | ((r, c), '.') -> Some ((r, c), Empty)
        | ((r, c), 'G') -> Some ((r, c), Goblin initHitPoints)
        | ((r, c), 'E') -> Some ((r, c), Elf initHitPoints)
        | x ->
            eprintfn "Unknown input [%A]" x
            None)
    |> Map.ofSeq


//////////////////////////////////////////////////////////////////////

let dumpMap (battlefield : Battlefield) =
    let toChar = function
        | Empty -> '.'
        | Goblin _ -> 'G'
        | Elf _ -> 'E'
        | Wall -> '#'

    let maxCol =
        battlefield
        |> Map.toSeq
        |> Seq.map (fun ((_, c), _) -> c) 
        |> Seq.max 
        |> (+) 1

    let maxRow =
        battlefield 
        |> Map.toSeq
        |> Seq.map (fun ((r, _), _) -> r)
        |> Seq.max
        |> (+) 1

    let dumpLine r =
        [0..maxCol]
        |> Seq.iter (fun c ->
            battlefield
            |> Map.tryFind (r, c)
            |> Option.defaultValue Wall
            |> toChar
            |> printf "%c")

        printfn ""

    [0..maxRow]
    |> Seq.iter dumpLine

let dumpStatus (battlefield, status) =
    printfn "Current Status: %A" status
    printfn "Current Battlefield:"
    dumpMap battlefield

dumpStatus (input, Fighting)

//////////////////////////////////////////////////////////////////////

let neighbors (r, c) = [(r - 1, c); (r, c - 1); (r, c + 1); (r + 1, c)]

let neighborsOfKind predicate battlefield pos =
    pos
    |> neighbors
    |> List.choose (fun pos' ->
        match Map.tryFind pos' battlefield with
        | None -> None
        | Some cell -> if (predicate cell) then Some pos' else None)

let emptyNeighbors = neighborsOfKind (function | Empty -> true | _ -> false)
let elfNeighbors = neighborsOfKind (function | Elf _ -> true | _ -> false)
let goblinNeighbors = neighborsOfKind (function | Goblin _ -> true | _ -> false)

let bfs battlefield start goal =

    let rec step visited queue =
        let (cur, path) = List.head queue
        if cur = goal then
            Some (List.rev path)
        else
            let potentialNeighbors =
                cur
                |> emptyNeighbors battlefield
                |> List.filter (fun pos -> not (Set.contains pos visited || List.exists (fun (elem, _) -> elem = pos) queue))
                |> List.map (fun pos -> (pos, pos :: path))
            let newQueue = List.append (List.tail queue) potentialNeighbors

            if List.isEmpty newQueue then None else step (Set.add cur visited) newQueue

    step Set.empty [(start, [start])]

let getCell battlefield pos = Map.tryFind pos battlefield

let findFirstCell foo xs =
    xs
    |> Seq.choose (fun pos ->
        match foo pos with
        | None -> None
        | Some path -> Some (List.length path, pos))
    |> Seq.sort
    |> Seq.tryHead
    |> Option.bind (snd >> Some)

let findMoveTarget battlefield myPos =
    let me = getCell battlefield myPos

    let acceptableTargetFilter =
        match me with
        | None -> failwith "where am I?"
        | Some Empty -> failwith "ooooooOOOOoooo, I'm a spooky ghost"
        | Some Wall -> failwith "someone page the Pegasus, I've phased into the walls"
        | Some (Elf _) -> function | Goblin _ -> true | _ -> false
        | Some (Goblin _) -> function | Elf _ -> true | _ -> false

    let adjacentEnemies =
        match me with
        | Some (Elf _) -> goblinNeighbors battlefield myPos
        | Some (Goblin _) -> elfNeighbors battlefield myPos
        | _ -> failwith "lol"

    if not (Seq.isEmpty adjacentEnemies) then
        (None, Fighting)
    else
        let allEnemies =
            battlefield
            |> Map.toSeq
            |> Seq.filter (fun (_, v) -> acceptableTargetFilter v)

        if Seq.isEmpty allEnemies then
            (None, CombatEnded)
        else
            allEnemies
            |> Seq.collect (fst >> emptyNeighbors battlefield)
            |> Seq.distinct
            |> findFirstCell (fun pos -> bfs battlefield myPos pos)
            |> fun cell -> (cell, Fighting)

let findMove battlefield myPos =
    match findMoveTarget battlefield myPos with
    | (_, CombatEnded)  -> (None, CombatEnded)
    | (_, MissionFailed)  -> (None, MissionFailed)
    | (None, Fighting) -> (None, Fighting)
    | (Some target, _) ->
        let nextSteps = emptyNeighbors battlefield myPos

        if Seq.contains target nextSteps then
            (Some target, Fighting)
        else
            nextSteps
            |> findFirstCell (fun pos -> bfs battlefield pos target)
            |> fun cell -> (cell, Fighting)

let doMove battlefield myPos =
    let me = getCell battlefield myPos |> Option.defaultValue Empty

    match findMove battlefield myPos with
    | (_, CombatEnded) -> (battlefield, myPos, CombatEnded)
    | (_, MissionFailed) -> (battlefield, myPos, MissionFailed)
    | (None, Fighting) -> (battlefield, myPos, Fighting)
    | (Some target, _) ->
        battlefield
        |> Map.add myPos Empty
        |> Map.add target me
        |> fun bf -> (bf, target, Fighting)

let findAttackTarget battlefield myPos =
    printfn "%A (%A) is finding attack target" myPos (getCell battlefield myPos)

    match getCell battlefield myPos with
    | None -> None
    | Some me ->

        let acceptableTargetFilter =
            match me with
            | Empty -> failwith "ooooooOOOOoooo, I'm a spooky ghost"
            | Wall -> failwith "someone page the Pegasus, I've phased into the walls"
            | Elf _ -> function | Goblin _ -> true | _ -> false
            | Goblin _ -> function | Elf _ -> true | _ -> false

        let adjacentEnemies =
            match me with
            | Elf _ ->
                goblinNeighbors battlefield myPos
                |> Seq.map (fun pos ->
                    match getCell battlefield pos with
                    | Some (Goblin hp) -> (hp, pos)
                    | _ -> failwith "lolwut")
            | Goblin _ ->
                elfNeighbors battlefield myPos
                |> Seq.map (fun pos ->
                    match getCell battlefield pos with
                    | Some (Elf hp) -> (hp, pos)
                    | _ -> failwith "lolwut")
            | _ -> failwith "lol"

        adjacentEnemies
        |> Seq.sort
        |> Seq.tryHead
        |> function | Some (_, pos) -> Some pos | _ -> None

let doAttack elfAttackPower battlefield myPos =
    printfn "Running attack from %A" myPos
    let me = getCell battlefield myPos
    let targetPos = findAttackTarget battlefield myPos
    match targetPos with
    | None -> (battlefield, Fighting)
    | Some targetPos' ->
        let target = getCell battlefield targetPos'

        let hitTarget =
            match target with
            | Some (Elf hp) ->
                if hp <= goblinAttackPower then
                    Empty
                else
                    Elf (hp - goblinAttackPower)
            | Some (Goblin hp) ->
                if hp <= elfAttackPower then
                    Empty
                else
                    Goblin (hp - elfAttackPower)
            | Some x -> x
            | None -> failwith "broken"

        printfn "hit target: %A (%A -> %A)" targetPos' target hitTarget

        let newBattlefield = 
            match me with
            | Some (Elf _) | Some (Goblin _) -> battlefield |> Map.add targetPos' hitTarget
            | _ -> battlefield

        match (target, hitTarget) with
        | Some (Elf _), Empty -> newBattlefield, MissionFailed
        | _ -> newBattlefield, Fighting

let doRound elfAttackPower battlefield =
    let fighterPos =
        battlefield
        |> Map.filter (fun _ -> function | Elf _ | Goblin _ -> true | _ -> false)
        |> Map.toSeq
        |> Seq.map fst

    printfn "%A" fighterPos

    let doFighterRound (bf, status) pos =
        printfn "%A (%A) has initiative:" pos (getCell bf pos)
        match status with
        | CombatEnded -> bf, CombatEnded
        | MissionFailed -> bf, MissionFailed
        | Fighting ->
            match getCell bf pos with
            | Some (Goblin _) | Some (Elf _) ->
                let bf', pos', status' = doMove bf pos
                match status' with
                | Fighting -> 
                    let bf'', status'' = doAttack elfAttackPower bf' pos'
                    bf'', status''
                | CombatEnded ->
                    printfn "Combat marked as ended"
                    bf', CombatEnded
                | MissionFailed ->
                    printfn "Combat ended with loss of elf"
                    bf', MissionFailed

            | _ -> (bf, Fighting)

    Seq.fold doFighterRound (battlefield, Fighting) fighterPos


let doBattle elfAttackPower battlefield =
    let rec step bf steps =
        printfn "Situation after %A rounds:" steps
        dumpMap bf

        let bf', status = doRound elfAttackPower bf
        match status with
        | Fighting -> step bf' (steps + 1)
        | CombatEnded ->
            printfn "Combat ended after %A rounds" steps
            (CombatEnded, bf', steps)
        | MissionFailed ->
            printfn "Combat ended after %A rounds due to elf death" steps
            (MissionFailed, bf', steps)

    step battlefield 0

let doSimulation battlefield =
    let rec step elfAttackPower =
        match doBattle elfAttackPower battlefield with
        | (CombatEnded, bf, steps) -> (bf, steps)
        | (MissionFailed, _, _) -> step (elfAttackPower + 1)
        | (Fighting, _, _) -> failwith "how?"

    step 4

let result = doSimulation input

let calculateOutcome (battlefield, rounds) =
    let totalhp =
        battlefield
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.sumBy (function | Goblin hp | Elf hp -> hp | _ -> 0)

    printfn "%A" totalhp

    totalhp * rounds

printfn "%A" (calculateOutcome result)

