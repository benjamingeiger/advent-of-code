// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic 
open System.Text.RegularExpressions

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

let enumerateSeq (s : seq<'a>) = Seq.zip (Seq.initInfinite id) s

let split (delimiter : char) (s : string) =
    s.Split(delimiter, (StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries))
    |> Array.toList

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
let input = readLines "input.txt"

let inputParts = input |> splitSeq ((=) "") |> Seq.toList
let ruleText = inputParts.[0]
let messageText = inputParts.[1]

type Rule = Terminal of string | Nonterminal of int | Sequence of Rule list | Alternate of Rule list

let parseRule (s : string) =
    let (ruleNumber, bodyText) = s.Split ": " |> (fun a -> (int a.[0]), a.[1])

    let rec parseSubrule (s' : string) =
        let s'' = s'.Trim()
        if s''.[0] = '"' then Terminal (s''.[1..(s''.Length - 2)])
        else if s''.Contains '|' then Alternate (s'' |> split '|' |> List.map parseSubrule)
        else if s''.Contains ' ' then Sequence (s'' |> split ' ' |> List.map parseSubrule)
        else Nonterminal (int s'')

    (ruleNumber, parseSubrule bodyText)

let rules = ruleText |> Seq.map parseRule |> Map.ofSeq

// n = nonterminal to start from
let parseString rules (s : string) =
    let l = List.ofSeq s

    let rec first =
        let first' n =
            match (Map.tryFind n rules) with
            | None -> failwith (sprintf "invalid rule %d" n)
            | Some (Terminal x) -> Set.singleton x
            | Some (Nonterminal i) -> first i
            | Some (Sequence rs) -> List.head rs |> first
            | Some (Alternate rs) -> rs |> List.map first |> Set.unionMany

        memoize first'

    let rec go n cs =
        if not (Set.contains (List.head cs) (first n)) then None
        else
            match (Map.tryFind n rules) with
            | None -> failwith (sprintf "invalid rule %d" n)


    go 0 l

    
