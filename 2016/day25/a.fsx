// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

type RegisterAddress = A | B | C | D
type Storage = Map<RegisterAddress, int>
type Operand = Register of RegisterAddress | Value of int
type Operator =
    | CPY of Operand * Operand
    | INC of Operand
    | DEC of Operand
    | JNZ of Operand * Operand
    | TGL of Operand
    | OUT of Operand
    | NOP
type Program = List<Operator>

let run program initStorage =
    let rec step program storage ip =
        (*printfn "Processing: %A %A %A" ip (Map.tryFind ip program) storage*)

        match Map.tryFind ip program with
        | None -> storage
        | Some (CPY (Value x, Register y)) -> step program (Map.add y x storage) (ip + 1)
        | Some (CPY (Register x, Register y)) ->
            step program (Map.add y (Map.find x storage) storage) (ip + 1)
        | Some (INC (Register x)) -> step program (Map.add x ((Map.find x storage) + 1) storage) (ip + 1)
        | Some (DEC (Register x)) -> step program (Map.add x ((Map.find x storage) - 1) storage) (ip + 1)
        | Some (JNZ (x, y)) -> 
            let xValue =
                match x with
                | Value x' -> x'
                | Register x' -> Map.find x' storage

            let destination =
                match y with
                | Value y' -> ip + y'
                | Register y' -> ip + (Map.find y' storage)

            if xValue <> 0
            then step program storage destination
            else step program storage (ip + 1)
        | Some (TGL (a)) ->
            let offset = match a with Value b -> b | Register b -> Map.find b storage
            let newInst =
                match (Map.tryFind (ip + offset) program) with
                | Some (CPY (x, y)) -> JNZ (x, y)
                | Some (INC (x)) -> DEC (x)
                | Some (DEC (x)) -> INC (x)
                | Some (JNZ (x, y)) -> CPY (x, y)
                | Some (TGL (x)) -> INC (x)
                | Some (OUT (x)) -> INC (x)
                | Some (NOP) -> NOP
                | None -> NOP

            step (Map.add (ip + offset) newInst program) storage (ip + 1)
        | Some (OUT (x)) ->
            storage
        | Some (NOP) -> step program storage (ip + 1)
        | Some inst ->
            printfn "Invalid instruction at address %A [%A], skipping" ip inst
            step program storage (ip + 1)

    step program initStorage 0

let program =
    let parseOperand s =
        match s with
        | "a" | "A" -> Register A
        | "b" | "B" -> Register B
        | "c" | "C" -> Register C
        | "d" | "D" -> Register D
        | _ -> Value (int s)

    let parseLine (l : string) =
        let parts = l.Split([| " " |], StringSplitOptions.RemoveEmptyEntries)

        match parts.[0] with
        | "cpy" -> CPY (parseOperand parts.[1], parseOperand parts.[2])
        | "inc" -> INC (parseOperand parts.[1])
        | "dec" -> DEC (parseOperand parts.[1])
        | "jnz" -> JNZ (parseOperand parts.[1], parseOperand parts.[2])
        | "tgl" -> TGL (parseOperand parts.[1])
        | "out" -> OUT (parseOperand parts.[1])
        | _ -> failwith (sprintf "invalid line %A" l)

    input |> Seq.map parseLine |> Seq.indexed |> Map.ofSeq

// Hypothesis: the value of D gets output bit by bit, last bit first.
// So we need to find the first value of D that is "10" repeating.

Seq.initInfinite (fun i -> [(A, i); (B, 0); (C, 0); (D, 0)] |> Map.ofList)
|> Seq.map (run program)
|> Seq.indexed
|> Seq.filter (fun (i, m) -> (Map.find D m) = 0b101010101010)
|> Seq.head
|> fst
|> printfn "%A"

