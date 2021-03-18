// vim: set et ts=4 sw=4 list :

open System

let input = [192;69;168;160;78;1;166;28;0;83;198;2;254;255;41;12]

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

let result =
    input
    |> List.fold step ((Array.init 256 id), 0, 0)
    |> fun (a, _, _) -> a.[0] * a.[1]

printfn "%A" result
