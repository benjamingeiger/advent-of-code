// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

#load "../binheap.fsx"
open Binheap

type Cell = Open | KeepMoving | Goal of char | Wall

let map = [ ((1, 1), Open);
            ((1, 2), Open);
            ((1, 3), KeepMoving);
            ((1, 4), Open);
            ((1, 5), KeepMoving);
            ((1, 6), Open);
            ((1, 7), KeepMoving);
            ((1, 8), Open);
            ((1, 9), KeepMoving);
            ((1, 10), Open);
            ((1, 11), Open)
            ((2, 3), Goal 'A');
            ((3, 3), Goal 'A');
            ((2, 5), Goal 'B');
            ((3, 5), Goal 'B');
            ((2, 7), Goal 'C');
            ((3, 7), Goal 'C');
            ((2, 9), Goal 'D');
            ((3, 9), Goal 'D') ] |> Map.ofList

//  #############
//  #...........#
//  ###C#B#A#D###
//    #C#D#A#B#
//    #########
let pods = [ (('A', 1), (2, 7));
             (('A', 2), (3, 7));
             (('B', 1), (2, 5));
             (('B', 2), (3, 9));
             (('C', 1), (2, 3));
             (('C', 2), (3, 3));
             (('D', 1), (2, 9));
             (('D', 2), (3, 5)) ] |> Map.ofList

let movementCost = [ ('A', 1); ('B', 10); ('C', 100); ('D', 1000) ] |> Map.ofList

let range x y = if x > y then [y .. x] else [x .. y]

let route (gr, gc) (tr, tc) =
        ([ for r in (range gr 1) do (r, gc) ] @ [ for c in (range gc tc) do (1, c) ] @ [ for r in (range 1 tr) do (r, tc) ])
        |> List.distinct

let possibleMoves pods =
    let occupied p = pods |> Map.exists (fun _ v -> v = p)

    let movablePods =
        pods
        // remove any pods already home
        |> Map.filter (fun (name, _) (row, col) ->
            match (Map.find (row, col) map) with
            | Goal x when x = name -> 
                if row = 3 then false
                else
                    match pods |> Map.tryFindKey (fun _ v -> v = (3, col)) with
                    | None -> true
                    | Some (name', _) -> name' <> name
            | _ -> true)
        // remove any pods blocked in
        |> Map.filter (fun _ (row, col) ->
            not (row = 3 && Map.exists (fun _ (r', c') -> (r', c') = (2, col)) pods))
        |> Map.toList

    // find all possible targets
    let outTargets =
        map
        |> Map.filter (fun k v -> (match v with | Open -> true | _ -> false) && not (occupied k))
        |> Map.toList

    let inTargets =
        map
        |> Map.filter (fun (r, c) v ->
            (match v with | Goal _ -> true | _ -> false)
            && not (occupied (r, c))
            && if r = 2 then (occupied (r + 1, c)) else true)
        |> Map.toList

    let pairs = List.allPairs movablePods (inTargets @ outTargets)

    let validMoves =
        pairs
        |> List.choose (fun (((name, num), (pr, pc)), ((tr, tc), targetType)) ->
            if pr = 1 && tr = 1 then None
            else
                let cells = 
                    match targetType with
                    | Open -> route (pr, pc) (tr, tc)
                    | Goal _ -> route (tr, tc) (pr, pc)
                    | _ -> failwith "something broke"

                let obstacles =
                    cells
                    |> List.map (fun pos -> pods |> Map.exists (fun _ v -> v = pos))
                    |> List.filter id

                let result = Some ((Map.find name movementCost) * (List.length cells - 1), (name, num), (pr, pc), (tr, tc), cells)
                if List.length obstacles > 1 then None
                else
                    match targetType with
                    | Open -> result
                    | Goal slot when slot = name -> result
                    | _ -> None)

    validMoves

let doMove pods (_, (name, num), _, (tr, tc), _) = pods |> Map.add (name, num) (tr, tc)

let isEverybodyHome pods =
    let notHome =
        pods
        |> Map.filter (fun (name, _) (r, c) ->
            match map |> Map.find (r, c) with
            | Goal x when x = name -> false
            | _ -> true)
        |> Map.count

    notHome = 0

let doMoveSearch initPods =
    let rec step seen statePQ =
        if BinHeap.isEmpty statePQ then failwith "couldn't find a solution"
        else
            let (curCost, (pods, moves)) = BinHeap.findMin statePQ

            printfn "cost: %A" curCost

            if curCost = 12521 then
                pods
                |> Map.toList
                |> List.iter (printfn "%A")

            if isEverybodyHome pods then
                moves
                |> List.rev
                |> List.iter (printfn "%A")

                curCost
            else
                let statePQ' = BinHeap.deleteMin statePQ

                if Set.contains pods seen then step seen statePQ'
                else
                    let outcomes =
                        possibleMoves pods
                        |> List.map (fun (cost, (name, num), (pr, pc), (tr, tc), cells) ->
                            let newPods = doMove pods (cost, (name, num), (pr, pc), (tr, tc), cells)

                            (cost + curCost, (newPods, (name, num, (tr, tc), cells, cost) :: moves)))
                        |> List.fold (fun acc el -> BinHeap.insert el acc) statePQ'

                    step (Set.add pods seen) outcomes

    step Set.empty (BinHeap.singleton (0, (initPods, [])))

let result =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let returnValue =
        doMoveSearch pods

    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds

    returnValue

printfn "%A" result
