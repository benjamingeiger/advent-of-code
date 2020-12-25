// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic 

let enumerateSeq s = Seq.zip (Seq.initInfinite id) s

let memoize (f : 'a -> 'b) =
    let dict = new Dictionary<'a, 'b>()

    let memoizedFunc (input : 'a) =
        match dict.TryGetValue(input) with
        | true, x -> x
        | false, _ ->
            let answer = f input
            dict.Add(input, answer)
            answer

    memoizedFunc

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let cardPublicKey = Seq.head input |> bigint.Parse
let doorPublicKey = Seq.head (Seq.tail input) |> bigint.Parse

printfn "%A %A" cardPublicKey doorPublicKey

let findLoopSize pk =
    let rec go cur loop = 
        if loop % 1000 = 0 then printfn "%d" loop
        if cur = pk then loop
        else go ((cur * 7I) % 20201227I) (loop + 1)

    go 1I 0

let cardLoopSize = findLoopSize cardPublicKey
let doorLoopSize = findLoopSize doorPublicKey

printfn "%A" cardLoopSize
printfn "%A" doorLoopSize

let generateEncryptionKey cardPK doorLoop =
    let rec go loop cur =
        if loop = 0 then cur
        else go (loop - 1) ((cur * cardPK) % 20201227I)

    go doorLoop 1I

printfn "%A" (generateEncryptionKey cardPublicKey doorLoopSize)

(*printfn "%A" ((cardPublicKey * doorPublicKey) % 20201227I)*)
