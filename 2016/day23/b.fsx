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
    | NOP
type Program = List<Operator>

let run program =

    let checkForMult program ip =
        (*
        Looking specifically for this pattern:
            cpy b c
            inc a
            dec c
            jnz c -2
            dec d
            jnz d -5
       *)

        let getInst ip = Map.tryFind ip program

        match (getInst ip, getInst (ip + 1), getInst (ip + 2), getInst (ip + 3), getInst (ip + 4), getInst (ip + 5)) with
        | (Some (CPY (Register b1, Register c1)),
            Some (INC (Register a2)),
            Some (DEC (Register c3)),
            Some (JNZ (Register c4, Value v4)),
            Some (DEC (Register d5)),
            Some (JNZ (Register d6, Value v6))) ->
                if c1 = c3 && c1 = c4 && d5 = d6 && v4 = -2 && v6 = -5
                then Some (Register a2, Register b1, Register c1, Register d5)
                else None
        | _ -> None

    let rec step program storage ip =
        (*printfn "Processing: %A %A %A" ip (Map.tryFind ip program) storage*)

        match Map.tryFind ip program with
        | None -> storage
        | Some (CPY (Value x, Register y)) -> step program (Map.add y x storage) (ip + 1)
        | Some (CPY (Register x, Register y)) ->
            match checkForMult program ip with
            | Some (Register a, Register b, Register c, Register d) ->
                let newStorage =
                    storage
                    |> Map.add a ((Map.find a storage) + (Map.find b storage) * (Map.find d storage))
                    |> Map.add c 0
                    |> Map.add d 0
                step program newStorage (ip + 6)
            | _ -> step program (Map.add y (Map.find x storage) storage) (ip + 1)
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
                | Some (NOP) -> NOP
                | None -> NOP

            step (Map.add (ip + offset) newInst program) storage (ip + 1)
        | Some (NOP) -> step program storage (ip + 1)
        | Some inst ->
            printfn "Invalid instruction at address %A [%A], skipping" ip inst
            step program storage (ip + 1)

    let initStorage = [(A, 12); (B, 0); (C, 0); (D, 0)] |> Map.ofList

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
        | _ -> failwith (sprintf "invalid line %A" l)

    input |> Seq.map parseLine |> Seq.indexed |> Map.ofSeq

printfn "%A" (run program)
