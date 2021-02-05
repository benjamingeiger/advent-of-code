// vim: set et ts=4 sw=4 list :

open System

type Element = Strontium | Plutonium | Thulium | Ruthenium | Curium | Elerium | Dilithium
type Item = Generator of Element | Processor of Element
type Floor = Set<Item>
type State = int * int * Map<int, Floor>
type StateCount = Map<int, Set<State>>
type SeenRecord = int * (int * int) list

let isGenerator item = match item with Generator _ -> true | Processor _ -> false
let isProcessor item = match item with Generator _ -> false | Processor _ -> true
let extractElement item = match item with Generator e -> e | Processor e -> e

let processors f = f |> Set.filter isProcessor |> Set.map extractElement
let generators f = f |> Set.filter isGenerator |> Set.map extractElement
let unmatchedChips f = Set.difference (processors f) (generators f)
let floorIsValid f = (unmatchedChips f |> Set.isEmpty) || (generators f |> Set.isEmpty)
let stateIsValid (_, _, fs) = fs |> Map.forall (fun k v -> floorIsValid v)

let distance (steps, elevator, floors) =
    let floorDistance =
        floors
        |> Map.toSeq
        |> Seq.map ((fun (idx, f) -> (3 - idx) * (Set.count f)))
        |> Seq.fold (+) 0

    floorDistance + steps

let generateStates (steps, elevator, floors) =
    let currentFloor = Map.find elevator floors |> Set.toList

    let pairs = List.allPairs currentFloor currentFloor

    let moveItems dst (item1, item2) =
        let newSrc =
            Map.find elevator floors 
            |> Set.remove item1
            |> Set.remove item2

        let newDst =
            Map.find dst floors
            |> Set.add item1
            |> Set.add item2

        floors
        |> Map.add elevator newSrc
        |> Map.add dst newDst

    let upStates =
        if elevator = 3
        then []
        else
            pairs
            |> List.map (moveItems (elevator + 1))
            |> List.map (fun fs -> (steps + 1, elevator + 1, fs))

    let downStates =
        if elevator = 0
        then []
        else
            pairs
            |> List.map (moveItems (elevator - 1))
            |> List.map (fun fs -> (steps + 1, elevator - 1, fs))

    List.append upStates downStates
    |> List.filter stateIsValid

let initState =
    (0, 0, [
        (0, [Generator Strontium; Processor Strontium; Generator Plutonium; Processor Plutonium; Generator Elerium; Processor Elerium; Generator Dilithium; Processor Dilithium]);
        (1, [Generator Thulium; Generator Ruthenium; Processor Ruthenium; Generator Curium; Processor Curium]);
        (2, [Processor Thulium]);
        (3, [])] 
    |> List.map (fun (n, s) -> (n, Set.ofList s))
    |> Map.ofList)

let run initState =
    let isFinalState (_, e, fs) =
        e = 3 
            && Set.count (Map.find 0 fs) = 0
            && Set.count (Map.find 1 fs) = 0
            && Set.count (Map.find 2 fs) = 0

    let generateSeenState (_, e, fs) =
        let elementPairs =
            fs
            |> Map.toList
            |> List.collect (fun (i, xs) -> xs |> Set.toList |> List.map (fun x -> (i, x)))
            |> List.groupBy (fun (i, x) -> extractElement x)
            |> List.map (fun (_, items) ->
                match items with
                | (i, Generator _) :: (j, Processor _) :: [] -> (i, j)
                | (i, Processor _) :: (j, Generator _) :: [] -> (j, i)
                | _ -> failwith (sprintf "Unknown combination (%A)?" items))
            |> List.sort

        (e, elementPairs)

    let rec step (visited : Set<SeenRecord>) (toVisit : Set<int * State>) (currentState : State) =
        if isFinalState currentState then currentState
        else if Set.contains (generateSeenState currentState) visited
        then
            let nextState = Set.minElement toVisit

            step visited (Set.remove nextState toVisit) (snd nextState)
        else
            let newStates = 
                currentState
                |> generateStates
                |> List.map (fun state -> (distance state, state))
                |> Set.ofList
                |> Set.union toVisit 

            let nextState = Set.minElement newStates 

            step (Set.add (generateSeenState currentState) visited) (Set.remove nextState newStates) (snd nextState)

    step
        (Set.empty)
        (Set.empty)
        initState

printfn "%A" (run initState)
