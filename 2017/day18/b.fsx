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

type state =
    // program number
    | Ready of int64
    // storage, ip (input queue is necessarily empty)
    | Waiting of Map<string, int64> * int
    | Halted of Map<string, int64>

let run program state input =
    let numInstructions = List.length instructions

    let getValue storage = function
        | Register r -> match Map.tryFind r storage with Some v -> v | None -> 0L
        | Value v -> v

    let rec tick storage ip inputQueue outputQueue =
        let gv = getValue storage

        if ip < 0 || ip >= numInstructions then (Halted storage, outputQueue)
        else
            match List.item ip program with
            | SND x -> tick storage (ip + 1) inputQueue ((gv x) :: outputQueue)
            | SET (Register x' as x, y) ->
                tick (Map.add x' (gv y) storage) (ip + 1) inputQueue outputQueue
            | SET (Value _, _) -> failwith "attempting to assign to a literal"
            | ADD (Register x' as x, y) ->
                tick (Map.add x' ((gv x) + (gv y)) storage) (ip + 1) inputQueue outputQueue
            | ADD (Value _, _) -> failwith "attempting to assign to a literal"
            | MUL (Register x' as x, y) ->
                tick (Map.add x' ((gv x) * (gv y)) storage) (ip + 1) inputQueue outputQueue
            | MUL (Value _, _) -> failwith "attempting to assign to a literal"
            | MOD (Register x' as x, y) ->
                tick (Map.add x' ((gv x) % (gv y)) storage) (ip + 1) inputQueue outputQueue
            | MOD (Value _, _) -> failwith "attempting to assign to a literal"
            | RCV (Register x' as x) ->
                match inputQueue with
                | [] -> (Waiting (storage, ip), outputQueue)
                | v :: vs -> tick (Map.add x' v storage) (ip + 1) vs outputQueue
            | RCV (Value _) -> failwith "attempting to assign to a literal"
            | JGZ (x, y) ->
                tick storage (ip + (if (gv x) > 0L then (int (gv y)) else 1)) inputQueue outputQueue

    match state with
    | Ready p -> tick (Map.add "p" p Map.empty) 0 input []
    | Waiting (storage, ip) ->
        if List.isEmpty input
        then (state, [])
        else tick storage ip input []
    | Halted _ -> (state, [])

let attemptRun program =
    let rec go p0 input0 p1 input1 count1 =
        let (p0', output0) = run program p0 input0
        let (p1', output1) = run program p1 input1

        if output0 = [] && output1 = []
        then count1
        else go p0' (List.rev output1) p1' (List.rev output0) (count1 + List.length output1)

    go (Ready 0L) [] (Ready 1L) [] 0

let result = attemptRun instructions

printfn "%A" result
