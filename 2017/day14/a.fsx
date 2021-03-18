// vim: set et ts=4 sw=4 list :

open System

let input = "jxqlasbh"

let generateHash key =
    let generateInput key =
        key
        |> Seq.map int
        |> Array.ofSeq
        |> fun a -> Array.concat [|a; [| 17; 31; 73; 47; 23 |] |]
        |> Array.replicate 64
        |> Array.concat

    let flipStartOfArray n a =
        Array.permute (fun i -> if i < n then n - i - 1 else i) a

    let rotateArray n a =
        let l = Array.length a
        Array.permute (fun i -> (l + i - n) % l) a

    let rotateStartingAt curPos length a =
        a
        |> rotateArray curPos
        |> flipStartOfArray length
        |> rotateArray -curPos

    let step (a, curPos, skipSize) length =
        let newA = rotateStartingAt curPos length a
        let newCurPos = (curPos + skipSize + length) % (Array.length a)
        let newSkipSize = skipSize + 1

        (newA, newCurPos, newSkipSize)

    key
    |> generateInput
    |> Seq.fold step ((Array.init 256 id), 0, 0)
    |> fun (a, _, _) -> a
    |> Array.chunkBySize 16
    |> Array.map (Array.fold (^^^) 0)
    |> Array.map (sprintf "%02x")
    |> String.concat ""

let countBits hexstr =
    hexstr
    |> Seq.map (function
        | '0' -> 0 | '1' -> 1 | '2' -> 1 | '3' -> 2
        | '4' -> 1 | '5' -> 2 | '6' -> 2 | '7' -> 3
        | '8' -> 1 | '9' -> 2 | 'a' -> 2 | 'b' -> 3
        | 'c' -> 2 | 'd' -> 3 | 'e' -> 3 | 'f' -> 4
        | _ -> -10000000)
    |> Seq.sum

[0..127]
|> List.map (sprintf "%s-%d" input)
|> List.map generateHash
|> List.map countBits
|> List.sum
|> printfn "%A"
