// vim: set et ts=4 sw=4 list :

open System

let uncurry f (a, b) = f a b

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.map bigint.Parse

let windowLength = 25

// borrowed from day 1
// (borrowed in turn from Stack Overflow)
let rec pairs = function
    | [] | [_] -> []
    | x :: xs -> 
        [for x' in xs do
            yield x,x'
         yield! pairs xs]

// Is target a valid next value, given the previous windowLength values?
let isValidNext (target, choices) =
    choices
    |> pairs
    // uncurry felt a bit more functional than a lambda expression
    |> List.map (uncurry (+))
    |> List.contains target

let segments =
    input
    // if our window is 5, grab 6 values (the previous 5 plus the current one)
    |> Seq.windowed (windowLength + 1)
    // separate target from the previous
    |> Seq.map Seq.rev
    |> Seq.map (fun xs -> (Seq.head xs, Seq.toList (Seq.tail xs)))
    // grab the ones that aren't valid
    |> Seq.filter (not << isValidNext)
    |> Seq.map fst

printfn "%A" segments
