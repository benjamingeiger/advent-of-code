// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic 
open System.Text.RegularExpressions

let memoize (f : 'a -> 'b) =
    let dict = new Dictionary<'a, 'b>()

    let memoizedFunc (input : 'a) =
        match dict.TryGetValue(input) with
        | true, x -> 
            printfn "skipping execution for %A = %A" input x
            x
        | false, _ ->
            printfn "running for %A" input
            let answer = f input
            dict.Add(input, answer)
            answer

    memoizedFunc

let enumerateSeq (s : seq<'a>) = Seq.zip (Seq.initInfinite id) s

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
let input = readLines "input.part2.txt"

let inputParts = input |> splitSeq ((=) "") |> Seq.toList
let ruleText = inputParts.[0]
let messageText = inputParts.[1]

(*printfn "%A" ruleText*)
(*printfn "%A" messageText*)

type Rule = Literal of string | Sequence of int list | Alternate of Rule list

let parseRule (s : string) =
    let (ruleNumber, bodyText) = s.Split ": " |> (fun a -> (int a.[0]), a.[1])

    let rec parseSubrule (s' : string) =
        let s'' = s'.Trim()
        if s''.[0] = '"' then Literal (s''.[1..(s''.Length - 2)])
        else if s''.Contains '|' then
            Alternate (s''.Split '|' |> Array.map parseSubrule |> Array.toList)
        else
            Sequence (s''.Split ' ' |> Array.map int |> Array.toList)

    (ruleNumber, parseSubrule bodyText)

(*printfn "%A" (Seq.head ruleText |> parseRule)*)

(*let rule8 =*)
    (*Alternate [*)
        (*Sequence [42; 42; 42; 42; 42; 42];*)
        (*Sequence [42; 42; 42; 42; 42];*)
        (*Sequence [42; 42; 42; 42];*)
        (*Sequence [42; 42; 42];*)
        (*Sequence [42; 42];*)
        (*Sequence [42]]*)

(*let rule11 =*)
    (*Alternate [*)
        (*Sequence [42; 42; 42; 42; 42; 31; 31; 31; 31; 31];*)
        (*Sequence [42; 42; 42; 42; 31; 31; 31; 31];*)
        (*Sequence [42; 42; 42; 31; 31; 31];*)
        (*Sequence [42; 42; 31; 31];*)
        (*Sequence [42; 31]]*)

let rules =
    ruleText
    |> Seq.map parseRule
    |> Map.ofSeq
    (*|> Map.add 8 rule8*)
    (*|> Map.add 11 rule11*)

(*printfn "%A" rules*)

let crossAppend (s1 : Set<string>) s2 =
    List.allPairs (Set.toList s1)  (Set.toList s2)
    |> List.map (fun (a, b) -> a + b)
    |> Set.ofList

let rec generateStrings =
    let rec getRule = function
        | n ->
            printfn "retrieving rule %d" n
            Map.find n rules

    and go rule =
        printfn "%A" rule
        let result = 
            match rule with
            | Literal s -> set [ s ]
            | Alternate rs -> Set.unionMany (List.map generateStrings rs)
            | Sequence ns -> ns |> List.map getRule |> List.map generateStrings |> List.fold crossAppend (set [ "" ])

        (*printfn "%A: %A" rule result*)

        result

    memoize go

let strings42 = generateStrings (Map.find 42 rules) |> Set.toList
let strings31 = generateStrings (Map.find 31 rules) |> Set.toList

printfn "strings42: %A" strings42
printfn "strings31: %A" strings31

(*printfn "%A" (Set.map String.length strings42)*)
(*printfn "%A" (Set.map String.length strings31)*)

// 0: 8 11
// 8: 42 | 42 8
// 11: 42 31 | 42 11 31

let matchString s =
    let startswith (prefix : string) (s' : string) = if s'.StartsWith(prefix) then Some (s'.[(String.length prefix)..]) else None
    let endswith (prefix : string) (s' : string) = if s'.EndsWith(prefix) then Some (s'.[..((String.length s') - (String.length prefix) - 1)]) else None

    let matches42 s =
        strings42
        |> List.map (fun p -> startswith p s)
        |> List.filter (Option.isSome)
        |> fun l ->
            match l with
            | [] -> None
            | [Some r] -> Some r
            | _ -> failwith "multiple matches? lolwut"

    let matches31 s =
        strings31
        |> List.map (fun p -> endswith p s)
        |> List.filter (Option.isSome)
        |> fun l ->
            match l with
            | [] -> None
            | [Some r] -> Some r
            | _ -> failwith "yeah no multiple matches here either"

    (*let matches8 = function*)
        (*| "" -> (false, "")*)
        (*| s' ->*)
            (*match matches42 s' with*)
            (*| None -> (false, "")*)
            (*| Some s'' -> (true, s'')*)

    (*let matches11 = function*)
        (*| "" -> (false, "")*)
        (*| s' ->*)
            (*match matches42 s' with*)
            (*| None -> (false, "")*)
            (*| Some s'' ->*)
                (*match matches31 s'' with*)
                (*| None -> (false, "")*)
                (*| Some s''' -> (true, s''')*)

    let rec matches0inner = function
        | "" -> true
        | s' ->
            match matches42 s' with
            | None -> false
            | Some s'' -> matches0inner s''

    let rec matches0outer' = function
        | "" -> false
        | s' ->
            match matches31 s' with
            | None -> matches0inner s'
            | Some s'' -> 
                match matches42 s'' with
                | None -> false
                | Some s''' -> matches0outer' s'''

    let rec matches0outer = function
        | "" -> false
        | s' ->
            match matches31 s' with
            | None -> false
            | Some s'' ->
                match matches42 s'' with
                | None -> false
                | Some s''' -> matches0outer' s'''

    matches0outer s

let results =
    input
    |> Seq.filter matchString
    |> Seq.toList
    |> List.length
    |> printfn "%A"
