// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.head

let countGarbage cs =
    let step (output, inGarbage, cancelNext, garbageCount) c =
        if cancelNext then (output, inGarbage, false, garbageCount)
        elif inGarbage then
            if c = '>' then (output, false, false, garbageCount)
            elif c = '!' then (output, true, true, garbageCount)
            else (output, true, false, garbageCount + 1)
        elif c = '<' then (output, true, false, garbageCount)
        else (c :: output, false, false, garbageCount)

    cs
    |> Seq.toList
    |> List.fold step ([], false, false, 0)
    |> fun (_, _, _, gc) -> gc

printfn "%A" (countGarbage input)
