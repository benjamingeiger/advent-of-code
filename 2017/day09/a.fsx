// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.head

let stripGarbage cs =
    let step (output, inGarbage, cancelNext) c =
        if cancelNext then (output, inGarbage, false)
        elif inGarbage then
            if c = '>' then (output, false, false)
            elif c = '!' then (output, true, true)
            else (output, true, false)
        elif c = '<' then (output, true, false)
        else (c :: output, false, false)
    
    cs
    |> Seq.toList
    |> List.fold step ([], false, false)
    |> fun (cs', _, _) -> cs'
    |> List.rev

printfn "%A" (stripGarbage input |> List.map string |> String.concat "")

let countScore cs =
    let step (score, depth) c =
        match c with
        | '{' -> (score, depth + 1)
        | '}' -> (score + depth, depth - 1)
        | ',' -> (score, depth)
        | c' ->
            printfn "Unknown character %A" c'
            (score, depth)

    cs
    |> Seq.toList
    |> List.fold step (0, 0)
    |> fst

printfn "%A" (countScore (stripGarbage input))
