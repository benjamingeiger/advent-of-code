// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic

#load "../utils.fsx"
open Utils

#load "../binheap.fsx"
open Binheap

type Amphipod = A | B | C | D
type Space = Space0 | Space1 | Space2 | Space3 | Space4 | Space5 | Space6
type Room = RoomA | RoomB | RoomC | RoomD

type State =
    {
        spaces: Map<Space, Amphipod option>
        rooms: Map<Room, (Amphipod * bool) * Amphipod list>
    }

let costPerUnit = function
    | A -> 1
    | B -> 10
    | C -> 100
    | D -> 1000

//   01 2 3 4 56
//  #############
//  #...........#
//  ###C#B#A#D###
//    #D#C#B#A#
//    #D#B#A#C#
//    #C#D#A#B#
//    #########

let initState =
    {
        spaces = [ (Space0, None); (Space1, None); (Space2, None); (Space3, None); (Space4, None); (Space5, None); (Space6, None); ] |> Map.ofList
        rooms = [ (RoomA, ((A, false), [C; D; D; C])); (RoomB, ((B, false), [B; C; B; D])); (RoomC, ((C, false), [A; B; A; A])); (RoomD, ((D, false), [D; A; C; B])) ] |> Map.ofList
    }

(*let initState =*)
    (*{*)
        (*spaces = [ (Space0, Some D); (Space1, None); (Space2, None); (Space3, None); (Space4, None); (Space5, None); (Space6, None); ] |> Map.ofList*)
        (*rooms = [ (RoomA, ((A, true), [A; A; A; A])); (RoomB, ((B, true), [B; B; B; B])); (RoomC, ((C, true), [C; C; C; C])); (RoomD, ((D, true), [D; D; D])) ] |> Map.ofList*)
    (*}*)

let distance space room =
    match room with
    | RoomA -> match space with | Space0 -> 2 | Space1 -> 1 | Space2 -> 1 | Space3 -> 3 | Space4 -> 5 | Space5 -> 7 | Space6 -> 8
    | RoomB -> match space with | Space0 -> 4 | Space1 -> 3 | Space2 -> 1 | Space3 -> 1 | Space4 -> 3 | Space5 -> 5 | Space6 -> 6
    | RoomC -> match space with | Space0 -> 6 | Space1 -> 5 | Space2 -> 3 | Space3 -> 1 | Space4 -> 1 | Space5 -> 3 | Space6 -> 4
    | RoomD -> match space with | Space0 -> 8 | Space1 -> 7 | Space2 -> 5 | Space3 -> 3 | Space4 -> 1 | Space5 -> 1 | Space6 -> 2

let obstacles space room =
    match room with
    | RoomA ->
        match space with
        | Space0 -> [Space1]
        | Space1 -> []
        | Space2 -> []
        | Space3 -> [Space2]
        | Space4 -> [Space2; Space3]
        | Space5 -> [Space2; Space3; Space4]
        | Space6 -> [Space2; Space3; Space4; Space5]
    | RoomB ->
        match space with
        | Space0 -> [Space1; Space2]
        | Space1 -> [Space2]
        | Space2 -> []
        | Space3 -> []
        | Space4 -> [Space3]
        | Space5 -> [Space3; Space4]
        | Space6 -> [Space3; Space4; Space5]
    | RoomC ->
        match space with
        | Space0 -> [Space1; Space2; Space3]
        | Space1 -> [Space2; Space3]
        | Space2 -> [Space3]
        | Space3 -> []
        | Space4 -> []
        | Space5 -> [Space4]
        | Space6 -> [Space4; Space5]
    | RoomD ->
        match space with
        | Space0 -> [Space1; Space2; Space3; Space4]
        | Space1 -> [Space2; Space3; Space4]
        | Space2 -> [Space3; Space4]
        | Space3 -> [Space4]
        | Space4 -> []
        | Space5 -> []
        | Space6 -> [Space5]

let roomTarget = function | RoomA -> A | RoomB -> B | RoomC -> C | RoomD -> D
let roomGoal = function | A -> RoomA | B -> RoomB | C -> RoomC | D -> RoomD

let findPossibleMoves state =
    let roomIsFilling podType room = room |> List.forall ((=) podType)

    let movesIn =
        let doMove ((podSpace, pod), room) =
            let spaces' = state.spaces |> Map.add podSpace None
            let rooms' =
                let details, contents = Map.find room state.rooms
                state.rooms |> Map.add room (details, pod :: contents)

            { state with spaces = spaces'; rooms = rooms' }

        let pods =
            state.spaces
            |> Map.toList
            |> List.choose (fun (k, v) -> match v with Some v' -> Some (k, v') | None -> None)

        let rooms =
            state.rooms
            |> Map.toList
            |> List.choose (fun (k, ((podType, isFilling), contents)) ->
                if isFilling && List.length contents < 4 then
                    Some (k, podType, 4 - List.length contents)
                else
                    None)

        let validMoves =
            List.allPairs pods rooms
            |> List.filter (fun ((podSpace, pod), (room, roomType, depth)) ->
                    (roomType = pod) &&
                    (obstacles podSpace room
                    |> List.map (fun space -> state.spaces |> Map.find space)
                    |> List.forall ((=) None)))
            |> List.map (fun ((space, pod), (room, roomType, depth)) ->
                ((costPerUnit pod) * (depth + distance space room), doMove ((space, pod), room)))

        validMoves

    let movesOut =
        let doMove (space, room, pod) =
            let spaces' = state.spaces |> Map.add space (Some pod)
            let rooms' =
                let ((podType, isFilling), contents) = Map.find room state.rooms
                let contents' = List.tail contents
                let isFilling' = contents' |> List.forall ((=) podType)

                state.rooms |> Map.add room ((podType, isFilling'), contents')

            { state with spaces = spaces'; rooms = rooms' }

        let spaces =
            state.spaces
            |> Map.toList
            |> List.choose (fun (k, v) -> match v with None -> Some k | Some _ -> None)

        let rooms =
            state.rooms
            |> Map.toList
            |> List.choose (fun (k, ((podType, isFilling), contents)) ->
                if not isFilling then
                    Some ((k, 5 - List.length contents), List.head contents)
                else
                    None)

        let validMoves =
            List.allPairs spaces rooms
            |> List.filter (fun (space, ((room, depth), pod)) ->
                    obstacles space room
                    |> List.map (fun space -> state.spaces |> Map.find space)
                    |> List.forall ((=) None))
            |> List.map (fun (space, ((room, depth), pod)) ->
                ((costPerUnit pod) * (depth + distance space room), doMove (space, room, pod)))

        validMoves

    movesIn @ movesOut

let isFinished state =
    (state.spaces |> Map.forall (fun _ v -> v = None)) && (state.rooms |> Map.forall (fun _ v -> v |> fst |> snd))

let pathSearch initState =
    let rec step (seen : HashSet<State>) statePQ =
        let (curCost, state) = BinHeap.findMin statePQ
        let statePQ' = BinHeap.deleteMin statePQ

        if isFinished state then curCost
        elif seen.Contains(state) then
            step seen statePQ'
        else
            if seen.Count % 1000 = 0 then printfn "seen: %A cost: %A" seen.Count curCost
            let statePQ'' =
                findPossibleMoves state
                |> List.filter (fun (_, newState) -> not(seen.Contains(newState)))
                |> List.map (fun (stepCost, newState) -> stepCost + curCost, newState)
                |> List.fold (fun acc el -> BinHeap.insert el acc) statePQ'

            seen.Add(state) |> ignore

            step seen statePQ''

    step (new HashSet<State>()) (BinHeap.singleton (0, initState))

let result =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let returnValue = pathSearch initState

    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds

    returnValue

printfn "%A" result
