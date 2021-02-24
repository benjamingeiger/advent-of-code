// vim: set et ts=4 sw=4 list :

open System

let input = 371

let next offset cur n =
    if cur + offset >= n then (cur + offset) % n + 1 else cur + offset + 1

let results = 
    Seq.init 50000001 id
    |> Seq.skip 1
    |> Seq.scan (next input) 0
    |> Seq.indexed
    |> Seq.choose (fun (n, pos) -> if pos = 1 then Some n else None)
    |> Seq.last

printfn "%A" results
