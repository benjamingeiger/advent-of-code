// vim: set et ts=4 sw=4 list :

open System.Collections.Generic 
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let memoize (f : 'a -> 'b) =
    let dict = new Dictionary<'a, 'b>()

    let memoizedFunc (input : 'a) =
        match dict.TryGetValue(input) with
        | true, x -> x
        | false, _ ->
            let answer = f input
            dict.Add(input, answer)
            answer

    memoizedFunc

// borrowed from https://stackoverflow.com/questions/6736464/split-seq-in-f
let splitSeq p s =
    let i = ref 0
    s
    |> Seq.map (fun x ->
        if p x then incr i
        !i, x)
    |> Seq.filter (fun (i, x) -> (not << p) x)
    |> Seq.groupBy fst
    |> Seq.map (fun (_, b) -> Seq.map snd b)

let readLines = System.IO.File.ReadLines

let binaryToInt s = System.Convert.ToInt32(s, 2)

let commaInts (s : string) = s.Split(",") |> Seq.map int


// borrowed from https://stackoverflow.com/questions/1526046/f-permutations
let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)




let mapInvert map = map |> Map.toSeq |> Seq.map (fun (a, b) -> (b, a)) |> Map.ofSeq

let mapValues map = map |> Map.toList |> List.map snd

let consolidateDigits ds =
    let rec go total = function
    | [] -> total
    | h :: t -> go (total * 10 + h) t

    go 0 (List.ofSeq ds)


let indexCharacters f input =
    input
    |> Seq.map Seq.indexed
    |> Seq.indexed
    |> Seq.collect (fun (r, rs) -> rs |> Seq.map (fun (c, x) -> ((r, c), f x)))


type BfsFunctions<'pos, 'value, 'data, 'output when 'pos : comparison> = {
    neighbors : 'pos -> 'pos list
    isValidNeighbor : 'pos -> 'value -> bool
    isTarget : 'pos -> 'value -> bool
    updateCellData : 'pos -> 'data -> 'value -> ('pos * 'data)
    noMatch : Map<'pos, 'data> -> 'output
    foundMatch : 'pos -> 'data -> 'output
}

let floodFunctions : BfsFunctions<(int * int), int, int option, Set<int * int>> = {
    neighbors = fun (r, c) -> [ (r + 1, c); (r - 1, c); (r, c + 1); (r, c - 1) ]
    isValidNeighbor = fun _ _ -> true
    isTarget = fun _ _ -> false
    updateCellData = fun pos _ _ -> (pos, None)
    noMatch = Map.toSeq >> Seq.map fst >> Set.ofSeq
    foundMatch = fun _ _ -> Set.empty
}

let allPointsShortestPathFunctions : BfsFunctions<(int * int), int, (int * int) list, Map<(int * int), (int * int) list>> = {
    neighbors = fun (r, c) -> [ (r + 1, c); (r - 1, c); (r, c + 1); (r, c - 1) ]
    isValidNeighbor = fun _ _ -> true
    isTarget = fun _ _ -> false
    updateCellData = fun pos oldPath _ -> (pos, pos :: oldPath)
    noMatch = id
    foundMatch = fun _ _ -> Map.empty
}

let singlePointShortestPathFunctions : BfsFunctions<(int * int), int, (int * int) list, (int * int) list option> = {
    neighbors = fun (r, c) -> [ (r + 1, c); (r - 1, c); (r, c + 1); (r, c - 1) ]
    isValidNeighbor = fun _ _ -> true
    isTarget = fun _ _ -> false
    updateCellData = fun pos oldPath _ -> (pos, pos :: oldPath)
    noMatch = fun _ -> None
    foundMatch = fun _ data -> Some data
}

let bfs
        (fs : BfsFunctions<'pos, 'value, 'data, 'output>)
        (map : Map<'pos, 'value>)
        (start : 'pos * 'data) =

    let rec step searched = function
        | [] -> fs.noMatch searched
        | (pos, data) :: queue ->
            if Map.containsKey pos searched then
                step searched queue
            else
                let value = map |> Map.find pos

                if fs.isTarget pos value then fs.foundMatch pos data
                else
                    let potentialNeighbors =
                        pos
                        |> fs.neighbors
                        |> Seq.filter (fun pos' -> Map.containsKey pos' map)
                        |> Seq.filter (fun pos' -> not ((Map.containsKey pos' searched) || (List.contains pos' (List.map fst queue))))
                        |> Seq.filter (fun pos' -> fs.isValidNeighbor pos' (Map.find pos' map))
                        |> Seq.map (fun pos' -> fs.updateCellData pos' data value)
                        |> List.ofSeq

                    step (Map.add pos data searched) (queue @ potentialNeighbors)

    step (Map.empty) [start]

let dijkstra
        (nodes : seq<'v>)
        (edges : Map<('v * 'v), int>)
        (start : 'v) =

    let INFINITY = System.Int32.MaxValue

    (*let name = fst*)
    let distance = snd >> fst
    (*let prev = snd >> snd*)

    let init v = (v, (INFINITY, v))
    let nodeMap =
        nodes 
        |> Seq.map init
        |> Map.ofSeq
        |> Map.add start (0, start)

    let allInfinity map = map |> Map.forall (fun _ (d, _) -> d = INFINITY)

    let closest map = map |> Map.toSeq |> Seq.minBy distance

    let rec step processed unprocessed =
        if Map.isEmpty unprocessed then processed
        elif allInfinity unprocessed then processed
        else
            let (n, (d, p)) = closest unprocessed

            let updatedUnprocessed =
                unprocessed
                |> Map.remove n
                |> Map.map (fun n' (d', p') ->
                    match Map.tryFind (n, n') edges with
                    | Some x -> if d + x < d' then (d + x, n) else (d', p')
                    | None -> (d', p'))

            step (Map.add n (d, p) processed) updatedUnprocessed

    step Map.empty nodeMap
