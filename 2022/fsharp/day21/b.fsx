// vim: set et ts=4 sw=4 list :

open System
open Checked

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

type Monkey =
    | RawMonkey of bigint
    | InputMonkey
    | PlusMonkey of string * string
    | MinusMonkey of string * string
    | TimesMonkey of string * string
    | DivideMonkey of string * string

type Oper =
    | Plus
    | Minus
    | Times
    | Divide

type Expr =
    | Raw of bigint
    | Input
    | Operation of Oper * Expr * Expr

let parseInput rawInput =
    rawInput
    |> List.choose (function
        | Regex "humn:.*" [] -> Some ("humn", InputMonkey)
        | Regex "(.*): (-?\d+)" [name; value] -> Some ((name, RawMonkey (bigint (int value))))
        | Regex "(.*): ([^ ]+) \+ ([^ ]+)" [name; first; second] -> Some ((name, PlusMonkey (first, second)))
        | Regex "(.*): ([^ ]+) - ([^ ]+)" [name; first; second] -> Some ((name, MinusMonkey (first, second)))
        | Regex "(.*): ([^ ]+) \* ([^ ]+)" [name; first; second] -> Some ((name, TimesMonkey (first, second)))
        | Regex "(.*): ([^ ]+) / ([^ ]+)" [name; first; second] -> Some ((name, DivideMonkey (first, second)))
        | s -> eprintfn "Unknown monkey: %s" s; None)
    |> Map.ofList

let getMonkey allMonkeys name = 
    match allMonkeys |> Map.tryFind name with
    | Some foo -> foo
    | None -> failwithf "can't find monkey %s" name

let rec evaluate = function
    | Raw value -> Raw value
    | Input -> Input
    | Operation (op, left, right) ->
        let leftEval = evaluate left
        let rightEval = evaluate right
        match (leftEval, rightEval) with
        | (Raw left', Raw right') ->
            match op with
            | Plus -> Raw (left' + right')
            | Minus -> Raw (left' - right')
            | Times -> Raw (left' * right')
            | Divide -> Raw (left' / right')
        | _ -> Operation (op, leftEval, rightEval)

let rec buildInput expr =
    match expr with
    | Raw value -> failwithf "partially evaluated: %A" (Raw value)
    | Input -> (1.0m, 0.0m)
    | Operation (op, Raw value, right) ->
        let value' = decimal value
        let (curX1, curX0) = buildInput right
        match op with
        | Plus -> (curX1, value' + curX0)
        | Minus -> (curX1, value' - curX0)
        | Times -> (curX1 * value', curX0 * value')
        | Divide -> (curX1 / value', curX0 / value')
    | Operation (op, left, Raw value) ->
        let value' = decimal value
        let (curX1, curX0) = buildInput left
        match op with
        | Plus -> (curX1, curX0 + value')
        | Minus -> (curX1, curX0 - value')
        | Times -> (curX1 * value', curX0 * value')
        | Divide -> (curX1 / value', curX0 / value')
    | Operation (op, left, right) ->
        failwithf "Partially evaluated: %A" (Operation (op, left, right))


let rec buildTree allMonkeys = function
    | RawMonkey value -> Raw value
    | InputMonkey -> Input
    | PlusMonkey (first, second) -> Operation (Plus, (buildTree allMonkeys (getMonkey allMonkeys first)), (buildTree allMonkeys (getMonkey allMonkeys second)))
    | MinusMonkey (first, second) -> Operation (Minus, (buildTree allMonkeys (getMonkey allMonkeys first)), (buildTree allMonkeys (getMonkey allMonkeys second)))
    | TimesMonkey (first, second) -> Operation (Times, (buildTree allMonkeys (getMonkey allMonkeys first)), (buildTree allMonkeys (getMonkey allMonkeys second)))
    | DivideMonkey (first, second) -> Operation (Divide, (buildTree allMonkeys (getMonkey allMonkeys first)), (buildTree allMonkeys (getMonkey allMonkeys second)))

let run input =
    let (left, right) =
        match (getMonkey input "root") with
        | PlusMonkey (left, right) | MinusMonkey (left, right) | TimesMonkey (left, right) | DivideMonkey (left, right) -> (left, right)
        | RawMonkey _ -> failwith "root shouldn't be a raw monkey"
        | InputMonkey -> failwith "root shouldn't be human"

    let leftValue = buildTree input (getMonkey input left) |> evaluate
    let rightValue = buildTree input (getMonkey input right) |> evaluate

    match (leftValue, rightValue) with
    | (Raw target, expr) | (expr, Raw target) ->
        let (x1, x0) = buildInput expr

        // negate because we're computing what we need to add to the input to equal zero
        (((decimal target) - x0) / x1) * -1.0m |> bigint
    | x -> failwithf "bad input: %A" x

doProcess parseInput run rawInput
