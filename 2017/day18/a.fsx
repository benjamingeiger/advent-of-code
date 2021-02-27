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

type Operand = Register of string | Value of int64

type Instruction =
    | SND of Operand
    | SET of Operand * Operand
    | ADD of Operand * Operand
    | MUL of Operand * Operand
    | MOD of Operand * Operand
    | RCV of Operand
    | JGZ of Operand * Operand

let parseOperand = function
    | Regex "^([a-z])$" [register] -> Some (Register register)
    | Regex "^(-?\d+)$" [value] -> Some (Value (int64 value))
    | s ->
        printfn "Invalid operand %A" s
        None

let parseInstruction = function
    | Regex "snd ([a-z]|-?\d+)" [x] ->
        let x' = parseOperand x

        match x' with
        | Some x'' -> Some (SND (x''))
        | _ -> None

    | Regex "set ([a-z]) ([a-z]|-?\d+)" [x; y] ->
        let x' = parseOperand x
        let y' = parseOperand y

        match x', y' with
        | Some (Register _ as x''), Some y'' -> Some (SET (x'', y''))
        | _ -> None

    | Regex "add ([a-z]) ([a-z]|-?\d+)" [x; y] ->
        let x' = parseOperand x
        let y' = parseOperand y

        match x', y' with
        | Some (Register _ as x''), Some y'' -> Some (ADD (x'', y''))
        | _ -> None

    | Regex "mul ([a-z]) ([a-z]|-?\d+)" [x; y] ->
        let x' = parseOperand x
        let y' = parseOperand y

        match x', y' with
        | Some (Register _ as x''), Some y'' -> Some (MUL (x'', y''))
        | _ -> None

    | Regex "mod ([a-z]) ([a-z]|-?\d+)" [x; y] ->
        let x' = parseOperand x
        let y' = parseOperand y

        match x', y' with
        | Some (Register _ as x''), Some y'' -> Some (MOD (x'', y''))
        | _ -> None

    | Regex "rcv ([a-z]|-?\d+)" [x] ->
        let x' = parseOperand x

        match x' with
        | Some x'' -> Some (RCV (x''))
        | _ -> None

    | Regex "jgz ([a-z]|-?\d+) ([a-z]|-?\d+)" [x; y] ->
        let x' = parseOperand x
        let y' = parseOperand y

        match x', y' with
        | Some x'', Some y'' -> Some (JGZ (x'', y''))
        | _ -> None

    | s ->
        printfn "Invalid instruction [%A]" s
        None

let instructions = input |> Seq.choose parseInstruction |> List.ofSeq

printfn "%A" instructions

let run program =
    let numInstructions = List.length instructions

    let getValue storage = function
        | Register r -> match Map.tryFind r storage with Some v -> v | None -> 0L
        | Value v -> v

    let rec tick storage ip =
        let gv = getValue storage

        if ip < 0 || ip >= numInstructions then storage
        else
            match List.item ip program with
            | SND x -> tick (Map.add "snd" (gv x) storage) (ip + 1)
            | SET (Register x' as x, y) ->
                tick (Map.add x' (gv y) storage) (ip + 1)
            | SET (Value _, _) -> failwith "attempting to assign to a literal"
            | ADD (Register x' as x, y) ->
                tick (Map.add x' ((gv x) + (gv y)) storage) (ip + 1)
            | ADD (Value _, _) -> failwith "attempting to assign to a literal"
            | MUL (Register x' as x, y) ->
                tick (Map.add x' ((gv x) * (gv y)) storage) (ip + 1)
            | MUL (Value _, _) -> failwith "attempting to assign to a literal"
            | MOD (Register x' as x, y) ->
                tick (Map.add x' ((gv x) % (gv y)) storage) (ip + 1)
            | MOD (Value _, _) -> failwith "attempting to assign to a literal"
            | RCV x ->
                if (gv x) <> 0L then storage
                else tick storage (ip + 1)
            | JGZ (x, y) ->
                tick storage (ip + (if (gv x) > 0L then (int (gv y)) else 1))

    tick Map.empty 0

let output = run instructions

printfn "%A" output
