// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> Seq.head

type Packet =
    | Literal of int * bigint
    | Operator of int * int * Packet list

let parsePacket input =
    let hexToBits = function
        | '0' -> "0000"
        | '1' -> "0001"
        | '2' -> "0010"
        | '3' -> "0011"
        | '4' -> "0100"
        | '5' -> "0101"
        | '6' -> "0110"
        | '7' -> "0111"
        | '8' -> "1000"
        | '9' -> "1001"
        | 'A' | 'a' -> "1010"
        | 'B' | 'b' -> "1011"
        | 'C' | 'c' -> "1100"
        | 'D' | 'd' -> "1101"
        | 'E' | 'e' -> "1110"
        | 'F' | 'f' -> "1111"
        | x ->
            printfn "invalid hex %c" x
            ""

    let allBits = input |> Seq.map hexToBits |> String.concat ""

    let bitsToInt bs = bs |> Seq.fold (fun acc x -> acc * 2 + (if x = '0' then 0 else 1)) 0
    let bitsToBigint bs = bs |> Seq.fold (fun acc x -> acc * 2I + (if x = '0' then 0I else 1I)) 0I

    let rec parse bits =
        let parseByLength length bits =
            let rec go length' (subPackets, remaining) =
                if length' = 0 then (subPackets, remaining)
                else
                    let initLength = Seq.length remaining
                    let (subPacket, remaining') = parse remaining
                    let finalLength = Seq.length remaining'

                    go (length' + finalLength - initLength) ((subPacket :: subPackets), remaining')

            let (subPackets, remaining) = go length ([], bits)
            (List.rev subPackets, remaining)

        let parseByCount count bits =
            let rec go count' (subPackets, remaining) =
                if count' = 0 then (subPackets, remaining)
                else
                    let (subPacket, remaining') = parse remaining

                    go (count' - 1) (subPacket :: subPackets, remaining')

            let (subPackets, remaining) = go count ([], bits)
            (List.rev subPackets, remaining)

        let parseLiteralPayload bits =
            let rec go bits' total =
                let group = bits' |> Seq.take 5
                let running = total * 16I + (bitsToBigint (Seq.skip 1 group))
                if Seq.head group = '0' then (running, Seq.skip 5 bits')
                else go (Seq.skip 5 bits') running

            go bits 0I

        let version = bits |> Seq.take 3 |> bitsToInt
        let packetType = bits |> Seq.skip 3 |> Seq.take 3 |> bitsToInt

        if packetType = 4 then
            // literal, parse payload
            let (payload, remaining) = parseLiteralPayload (Seq.skip 6 bits)
            (Literal (version, payload), remaining)
        else
            // operator, parse subpackets
            let lengthType = Seq.skip 6 bits |> Seq.head
            if lengthType = '0' then
                let payloadLength = bits |> Seq.skip 7 |> Seq.take 15 |> bitsToInt

                let (subPackets, remaining) = bits |> Seq.skip 22 |> parseByLength payloadLength

                (Operator (version, packetType, subPackets), remaining)

            else
                let payloadLength = bits |> Seq.skip 7 |> Seq.take 11 |> bitsToInt

                let (subPackets, remaining) = bits |> Seq.skip 18 |> parseByCount payloadLength

                (Operator (version, packetType, subPackets), remaining)

    parse allBits

let rec versionSum = function
    | Literal (v, _) -> v
    | Operator (v, _, ps) -> v + List.sumBy versionSum ps

let result =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let packet = parsePacket input |> fst

    printfn "%A" packet

    let returnValue = versionSum packet

    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds

    returnValue

printfn "%A" result
