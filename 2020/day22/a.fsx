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

let readLines filePath = System.IO.File.ReadLines filePath
let input =
    readLines "input.txt"
        |> splitSeq ((=) "")
        |> Seq.map (Seq.tail)
        |> Seq.map (Seq.map int)
        |> Seq.map List.ofSeq
        |> List.ofSeq

let score deck =
    deck
        |> List.rev
        |> List.zip [1..(List.length deck)]
        |> List.map (fun (a, b) -> a * b)
        |> List.reduce (+)

let rec combat deck1 deck2 =
    printfn "deck1: %A" deck1
    printfn "deck2: %A" deck2
    match (deck1, deck2) with
    | ([], _) ->
        score deck2
    | (_, []) ->
        score deck1
    | (h1 :: t1, h2 :: t2) ->
        if h1 > h2 then combat (t1 @ [h1; h2]) (t2)
        else if h2 > h1 then combat (t1) (t2 @ [h2; h1])
        else failwith "tie?"

let result = combat input.[0] input.[1]

printfn "%A" result
