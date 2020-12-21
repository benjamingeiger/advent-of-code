// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic 

let enumerateSeq s = Seq.zip (Seq.initInfinite id) s

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

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> splitSeq ((=) "") |> Seq.map Seq.toList |> Seq.toList

let getPiece (image : string list) =
    let header = List.head image
    let number = (header.Split " ").[1] |> (fun l -> int l.[..(String.length l) - 2])
    let scanLines = List.tail image
    let top = List.head scanLines
    let bottom = List.last scanLines
    let left = String.concat "" (List.map (fun (l : string) -> string l.[0]) scanLines) 
    let right = String.concat "" (List.map (fun (l : string) -> string l.[String.length l - 1]) scanLines)

    (number, top, right, bottom, left)

let pieces = input |> List.map getPiece

let rotate (number, top, right, bottom, left) = number, left, top, right, bottom

let invert (s : string) = (s.ToCharArray() |> Array.rev |> System.String)
let flip (number, top, right, bottom, left) = (number, (invert top), left, (invert bottom), right)

let canonical edge =
    let bits (edge : string) =
        let rec bits' acc cs =
            match cs with
            | [] -> acc
            | h :: t -> bits' (acc * 2 + (if h = "#" then 1 else 0)) t

        bits' 0 (edge.ToCharArray() |> Array.toList |> List.map string)

    let forward = bits edge
    let reverse = bits (invert edge)

    if forward > reverse then edge else invert edge

// let's try this one first
let allEdgePatterns pieces =
    pieces 
    |> List.collect (fun (_, t, r, b, l) -> [t; r; b; l])
    |> List.map canonical
    |> List.sort
    |> List.rev
    |> Seq.groupBy id
    |> Seq.map (fun (k, v) -> k, (Seq.length v))

let unmatchedEdges =
    allEdgePatterns pieces
    |> Seq.filter (fun (k, c) -> c = 1)
    |> Seq.map fst
    |> Set.ofSeq

let isCornerPiece (number, top, right, bottom, left) =
    [top; right; bottom; left]
    |> List.map canonical
    |> List.filter (fun e -> Set.contains e unmatchedEdges)
    |> fun x -> List.length x = 2

pieces
|> List.filter isCornerPiece
|> List.map (fun (n, _, _, _, _) -> bigint n)
|> List.reduce (*)
|> printfn "%A"

