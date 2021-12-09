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

(*let bfsGenerator neighbors isTarget start cells =*)
    (*let rec step visited queue =*)
        (*let (cur, path) = List.head queue*)
        (*if isTarget cur then Some (List.rev path)*)
        (*else*)
            (*let potentialNeighbors*)



type BfsFunctions<'pos, 'value, 'data, 'output when 'pos : comparison> = {
    neighbors : 'pos -> 'pos list
    isValidNeighbor : 'pos -> 'value -> bool
    isTarget : 'pos -> 'value -> bool
    updateCellData : 'pos -> 'data -> 'value -> ('pos * 'data)
    noMatch : Map<'pos, 'data> -> 'output
    foundMatch : 'pos -> 'value -> 'output
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

                if fs.isTarget pos value then fs.foundMatch pos value
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





