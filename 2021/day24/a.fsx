// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> List.ofSeq

type Source = Literal of bigint | Register of char
type Instruction =
    | INP of char
    | ADD of char * Source
    | MUL of char * Source
    | DIV of char * Source
    | MOD of char * Source
    | EQL of char * Source

let parseInput input =
    input
    |> Seq.choose (function
        | Regex "^inp (.)$" [reg] -> Some (INP (reg.[0]))
        | Regex "^add (.) (-?\d+)$" [reg; lit] -> Some (ADD (reg.[0], Literal (bigint (int lit))))
        | Regex "^add (.) (.)$" [r1; r2] -> Some (ADD (r1.[0], Register (r2.[0])))
        | Regex "^mul (.) (-?\d+)$" [reg; lit] -> Some (MUL (reg.[0], Literal (bigint (int lit))))
        | Regex "^mul (.) (.)$" [r1; r2] -> Some (MUL (r1.[0], Register (r2.[0])))
        | Regex "^div (.) (-?\d+)$" [reg; lit] -> Some (DIV (reg.[0], Literal (bigint (int lit))))
        | Regex "^div (.) (.)$" [r1; r2] -> Some (DIV (r1.[0], Register (r2.[0])))
        | Regex "^mod (.) (-?\d+)$" [reg; lit] -> Some (MOD (reg.[0], Literal (bigint (int lit))))
        | Regex "^mod (.) (.)$" [r1; r2] -> Some (MOD (r1.[0], Register (r2.[0])))
        | Regex "^eql (.) (-?\d+)$" [reg; lit] -> Some (EQL (reg.[0], Literal (bigint (int lit))))
        | Regex "^eql (.) (.)$" [r1; r2] -> Some (EQL (r1.[0], Register (r2.[0])))
        | x ->
            printfn "bad instruction %s" x
            None)

let run code input =
    let proc (registers, input) = function
        | INP reg ->
            (*printfn "Reading digits, registers: %A" registers*)
            Map.add reg (List.head input) registers, List.tail input
        | ADD (reg, (Literal lit)) -> Map.add reg (lit + (Map.find reg registers)) registers, input
        | ADD (reg, (Register r)) -> Map.add reg ((Map.find r registers) + (Map.find reg registers)) registers, input
        | MUL (reg, (Literal lit)) -> Map.add reg (lit * (Map.find reg registers)) registers, input
        | MUL (reg, (Register r)) -> Map.add reg ((Map.find r registers) * (Map.find reg registers)) registers, input
        | DIV (reg, (Literal lit)) -> Map.add reg ((Map.find reg registers) / lit) registers, input
        | DIV (reg, (Register r)) -> Map.add reg ((Map.find reg registers) / (Map.find r registers)) registers, input
        | MOD (reg, (Literal lit)) -> Map.add reg ((Map.find reg registers) % lit) registers, input
        | MOD (reg, (Register r)) -> Map.add reg ((Map.find reg registers) % (Map.find r registers)) registers, input
        | EQL (reg, (Literal lit)) -> Map.add reg (if lit = (Map.find reg registers) then 1I else 0I) registers, input
        | EQL (reg, (Register r)) -> Map.add reg (if (Map.find r registers) = (Map.find reg registers) then 1I else 0I) registers, input

    Seq.fold proc ([('w', 0I); ('x', 0I); ('y', 0I); ('z', 0I)] |> Map.ofList, input) code

let result =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let parsed = parseInput input

    // Did this one with pencil and paper. Mostly due to the explanation here:
    // https://www.reddit.com/r/adventofcode/comments/rnejv5/2021_day_24_solutions/hps5hgw/

    // part 1 answer is the first result, part 2 is the last.
    let possibilities = seq {
        // w0 = 9
        for w1 in [9; 8; 7; 6; 5; 4; 3] do
        for w2 in [2; 1] do
        // w3 = w2 + 7
        for w4 in [9; 8; 7; 6; 5] do
        // w5 = w4 - 4
        // w6 = 1
        for w7 in [3; 2; 1] do
        for w8 in [8; 7; 6; 5; 4; 3; 2; 1] do
        // w9 = w8 + 1
        // w10 = w7 - 6
        // w11 = 9
        // w12 = w1 - 2
        // w13 = 1
            yield [9; w1; w2; w2 + 7; w4; w4 - 4; 1; w7; w8; w8 + 1; w7 + 6; 9; w1 - 2; 1] |> List.map bigint
    }

    possibilities
    |> Seq.map (fun input -> input, run parsed input)
    |> Seq.filter (fun (input, (registers, _)) -> registers |> Map.find 'z' = 0I)
    |> Seq.iter (printfn "%A")

    let returnValue = 0

    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds

    returnValue

printfn "%A" result
