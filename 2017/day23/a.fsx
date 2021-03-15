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
    | SET of Operand * Operand
    | SUB of Operand * Operand
    | MUL of Operand * Operand
    | JNZ of Operand * Operand

let parseOperand = function
    | Regex "^([a-z])$" [register] -> Some (Register register)
    | Regex "^(-?\d+)$" [value] -> Some (Value (int64 value))
    | s ->
        printfn "Invalid operand %A" s
        None

let parseInstruction = function
    | Regex "set ([a-z]) ([a-z]|-?\d+)" [x; y] ->
        let x' = parseOperand x
        let y' = parseOperand y

        match x', y' with
        | Some (Register _ as x''), Some y'' -> Some (SET (x'', y''))
        | _ -> None

    | Regex "sub ([a-z]) ([a-z]|-?\d+)" [x; y] ->
        let x' = parseOperand x
        let y' = parseOperand y

        match x', y' with
        | Some (Register _ as x''), Some y'' -> Some (SUB (x'', y''))
        | _ -> None

    | Regex "mul ([a-z]) ([a-z]|-?\d+)" [x; y] ->
        let x' = parseOperand x
        let y' = parseOperand y

        match x', y' with
        | Some (Register _ as x''), Some y'' -> Some (MUL (x'', y''))
        | _ -> None

    | Regex "jnz ([a-z]|-?\d+) ([a-z]|-?\d+)" [x; y] ->
        let x' = parseOperand x
        let y' = parseOperand y

        match x', y' with
        | Some x'', Some y'' -> Some (JNZ (x'', y''))
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

    let rec tick storage ip mulCount =
        let gv = getValue storage

        (*printfn "%A %A" (List.item ip program) storage*)

        if ip < 0 || ip >= numInstructions then (mulCount, storage)
        else
            match List.item ip program with
            | SET (Register x' as x, y) ->
                tick (Map.add x' (gv y) storage) (ip + 1) mulCount
            | SET (Value _, _) -> failwith "attempting to assign to a literal"
            | SUB (Register x' as x, y) ->
                tick (Map.add x' ((gv x) - (gv y)) storage) (ip + 1) mulCount
            | SUB (Value _, _) -> failwith "attempting to assign to a literal"
            | MUL (Register x' as x, y) ->
                tick (Map.add x' ((gv x) * (gv y)) storage) (ip + 1) (mulCount + 1)
            | MUL (Value _, _) -> failwith "attempting to assign to a literal"
            | JNZ (x, y) ->
                tick storage (ip + (if (gv x) <> 0L then (int (gv y)) else 1)) mulCount

    tick Map.empty 0 0

let output = run instructions

printfn "%A" output
