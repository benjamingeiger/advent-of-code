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

type Rule = Terminal of char | Nonterminal of int | Sequence of Rule list | Alternate of Rule list

let parseRule (s : string) =
    let (ruleNumber, bodyText) = s.Split ": " |> (fun a -> (int a.[0]), a.[1])

    let rec parseSubrule (s' : string) =
        let s'' = s'.Trim()
        if s''.[0] = '"' then Terminal (s''.[1])
        else if s''.Contains '|' then Alternate (s'' |> split '|' |> List.map parseSubrule)
        else if s''.Contains ' ' then Sequence (s'' |> split ' ' |> List.map parseSubrule)
        else Nonterminal (int s'')

    (ruleNumber, parseSubrule bodyText)

let rules = ruleText |> Seq.map parseRule |> Map.ofSeq

// https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/

type Result<'a> = Success of 'a | Failure of string
type Parser<'a> = Parser of (string -> Result<'a * string>)

let run parser input =
    let (Parser p) = parser
    p input

let parseTerminal c =
    let go s =
        if s = "" then Failure "no more input"
        else if s.[0] = c then Success (string c, s.[1..])
        else Failure (sprintf "Expecting %c, got %c" c s.[0])

    Parser go

let andThen p1 p2 =
    let go input =
        match run p1 input with
        | Failure err -> Failure err
        | Success (v1, r1) ->
            match run p2 r1 with
            | Failure err -> Failure err
            | Success (v2, r2) ->
                Success (v1 + v2, r2)

    Parser go

let orElse p1 p2 =
    let go input =
        match run p1 input with
        | Success (v1, r1) -> Success (v1, r1)
        | Failure err -> run p2 input
    
    Parser go

let buildParser rules =
    let rec go =
        let go' r =
            printfn "building parser for %A" r
            match r with
            | Terminal x -> parseTerminal x
            | Nonterminal n ->
                match Map.tryFind n rules with
                | None -> failwith (sprintf "invalid nonterminal %d" n)
                | Some r -> go r
            | Sequence rs -> rs |> List.map go |> List.reduce andThen
            | Alternate rs -> rs |> List.map go |> List.reduce orElse

        memoize go'

    match Map.tryFind 0 rules with
    | None -> failwith "wtf"
    | Some r -> go r

let parser = buildParser rules

printfn "%A" parser



