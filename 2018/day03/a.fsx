// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

type Dimension = Dimension of int * int
type Claim = int * (Dimension * Dimension)

let claims =
    let parseCoords x w =
        let x' = int x
        let w' = int w
        
        Dimension (x', x' + w' - 1)

    let parseLine = function
        | Regex "^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$" [index; x; y; w; h] ->
            Some (int index, (parseCoords x w, parseCoords y h))
        | _ -> 
            printfn "Invalid line"
            None

    Seq.choose parseLine input

let processClaims claims =
    let getClaimsForInch squareInches (x, y) =
        match Map.tryFind (x, y) squareInches with
        | Some (claims) -> claims
        | None -> Set.empty

    let processClaim squareInches = function
        | index, (Dimension (x1, x2), Dimension (y1, y2)) ->
            Seq.allPairs [x1..x2] [y1..y2]
            |> Seq.map (fun (x, y) -> ((x, y), Set.add index (getClaimsForInch squareInches (x, y))))
            |> Seq.fold (fun sq ((x, y), c) -> Map.add (x, y) c sq) squareInches

    Seq.fold processClaim Map.empty claims

let result =
    claims
    |> processClaims
    |> Map.filter (fun k v -> (Set.count v) > 1)
    |> Map.toSeq
    |> Seq.length

printfn "%A" result
