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

type Winner = Player1 of int list | Player2 of int list

let rec combat seenstates deck1 deck2 =
    if Set.contains (deck1, deck2) seenstates
    then Player1 deck1
    else
        let newstates = Set.add (deck1, deck2) seenstates
        match (deck1, deck2) with
        | ([], _) ->
            Player2 deck2
        | (_, []) ->
            Player1 deck1
        | (h1 :: t1, h2 :: t2) ->
            if h1 <= (List.length t1) && h2 <= (List.length t2) then
                match combat Set.empty (List.take h1 t1) (List.take h2 t2) with
                | Player1 _ -> combat newstates (t1 @ [h1; h2]) t2
                | Player2 _ -> combat newstates t1 (t2 @ [h2; h1])
            else if h1 > h2 then combat newstates (t1 @ [h1; h2]) (t2)
            else if h2 > h1 then combat newstates (t1) (t2 @ [h2; h1])
            else failwith "tie?"

let result =
    match combat Set.empty input.[0] input.[1] with
    | Player1 l | Player2 l -> score l

printfn "%A" result
