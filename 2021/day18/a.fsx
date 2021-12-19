// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

type Snailfish =
    | Regular of int
    | Pair of Snailfish * Snailfish

let input = readLines "input.txt" |> Seq.toList

type parseStates =
    | Neither
    | Left of Snailfish
    | Complete of Snailfish

let parseInput input =
    let parseSnailfish states c =
        match (states, c) with
        | _, '[' -> Neither :: states
        | _, d when d >= '0' && d <= '9' -> Complete (Regular (int d - int '0')) :: states
        | Complete left :: Neither :: rest, ',' -> Left left :: rest
        | Complete right :: Left left :: rest, ']' -> Complete (Pair (left, right)) :: rest
        | _, _ -> failwithf "Unmatched state: %A, %A" states c

    let parseInputLine line = line |> Seq.fold parseSnailfish []

    input
    |> List.map parseInputLine
    |> List.map (function
        | [Complete sailfish] -> sailfish
        | _ -> failwith "incomplete parse")

// http://learnyouahaskell.com/zippers
type Crumb = LeftCrumb of Snailfish | RightCrumb of Snailfish
type Breadcrumbs = Crumb list
type FishZipper = Snailfish * Breadcrumbs

let goLeft (fish, crumbs) =
    match fish with
    | Regular _ ->
        failwith "can't split a fish"
    | Pair (left, right) -> (left, RightCrumb right :: crumbs)

let goRight (fish, crumbs) =
    match fish with
    | Regular _ ->
        failwith "can't split a fish"
    | Pair (left, right) -> (right, LeftCrumb left :: crumbs)

let goUp (fish, crumbs) =
    match crumbs with
    | [] -> failwith "can't go above the top"
    | LeftCrumb left :: rest -> (Pair (left, fish), rest)
    | RightCrumb right :: rest -> (Pair (fish, right), rest)

let rec unzip (fish, crumbs) =
    match crumbs with
    | [] -> fish
    | _ -> unzip (goUp (fish, crumbs))

let replaceWith newFish (fish, crumbs) = (newFish, crumbs)

let addTo addedValue (fish, crumbs) =
    match fish with
    | Regular x -> (fish, crumbs) |> replaceWith (Regular (x + addedValue))
    | Pair _ -> failwith "trying to add to a pair?"

let findSuccessor (fish, crumbs) =
    let rec findRightAncestor (fish, crumbs) =
        match crumbs with
        | [] -> None
        | LeftCrumb _ :: _ -> findRightAncestor (goUp (fish, crumbs))
        | RightCrumb _ :: _ -> Some (goUp (fish, crumbs))

    let rec findLeftDescendant (fish, crumbs) =
        match fish with
        | Regular x -> (fish, crumbs)
        | Pair (left, right) -> findLeftDescendant (goLeft (fish, crumbs))

    (fish, crumbs)
    |> findRightAncestor
    |> Option.map goRight
    |> Option.map findLeftDescendant

let findPredecessor (fish, crumbs) =
    let rec findLeftAncestor (fish, crumbs) =
        match crumbs with
        | [] -> None
        | LeftCrumb _ :: _ -> Some (goUp (fish, crumbs))
        | RightCrumb _ :: _ -> findLeftAncestor (goUp (fish, crumbs))

    let rec findRightDescendant (fish, crumbs) =
        match fish with
        | Regular x -> fish, crumbs
        | Pair (left, right) -> findRightDescendant (goRight (fish, crumbs))

    (fish, crumbs)
    |> findLeftAncestor
    |> Option.map goLeft
    |> Option.map findRightDescendant

let addToPredecessor x (fish, crumbs) =
    (fish, crumbs)
    |> findPredecessor
    |> Option.map (addTo x)
    |> Option.bind findSuccessor
    |> Option.defaultValue (fish, crumbs)

let addToSuccessor x (fish, crumbs) =
    let returnValue =
        (fish, crumbs)
        |> findSuccessor
        |> Option.map (addTo x)
        |> Option.bind findPredecessor
        |> Option.defaultValue (fish, crumbs)

    returnValue

let add red blue =
    let initialSum = Pair (red, blue)

    let rec reduce fish =
        let zipper = (fish, [])

        let rec findExplosion depth (fish, crumbs) =
            if depth > 0 then
                match fish with
                | Regular _ -> (false, (fish, crumbs))
                | Pair (_, _) ->
                    let leftZipper = goLeft (fish, crumbs)
                    let (leftModified, leftResult) = findExplosion (depth - 1) leftZipper
                    if not leftModified then
                        let rightZipper = goRight (fish, crumbs)
                        let (rightModified, rightResult) = findExplosion (depth - 1) rightZipper
                        if not rightModified then
                            (false, (fish, crumbs))
                        else
                            (true, goUp rightResult)
                    else
                        (true, goUp leftResult)
            else
                match fish with
                | Regular x -> (false, goUp (fish, crumbs))
                | Pair (Regular x, Regular y) ->
                    let exploded =
                        (fish, crumbs)
                        |> replaceWith (Regular 0)
                        |> addToPredecessor x
                        |> addToSuccessor y
                    (true, exploded)
                | Pair (_, _) -> failwith "too deep to splode"

        let rec findSplit (fish, crumbs) =
            match fish with
            | Regular x -> if x >= 10 then (true, (fish, crumbs) |> replaceWith (Pair (Regular (x / 2), Regular (x - (x / 2))))) else (false, (fish, crumbs))
            | Pair (x, y) ->
                let leftModified, leftZipper = (fish, crumbs) |> goLeft |> findSplit
                if leftModified then
                    (leftModified, goUp leftZipper)
                else
                    let rightModified, rightZipper = (fish, crumbs) |> goRight |> findSplit
                    (rightModified, goUp rightZipper)

        let didExplode, explosionZipper = findExplosion 4 zipper
        if not didExplode then
            let didSplit, splitZipper = findSplit zipper
            if not didSplit then
                unzip zipper
            else
                reduce (unzip splitZipper)
        else
            reduce (unzip explosionZipper)

    reduce initialSum

let rec magnitude = function
    | Regular x -> x
    | Pair (x, y) -> 3 * (magnitude x) + 2 * (magnitude y)

let result =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let returnValue = parseInput input |> List.reduce add |> magnitude

    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds

    returnValue

printfn "%A" result
