// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.head

let parse (s : string) = s.Split() |> Array.map int |> Array.toList

let findLargest l = List.findIndex ((=) (List.max l)) l

let distribute banks n =
    let rec go (bs : int[]) toDistribute cur =
        if toDistribute = 0 then bs
        else
            bs.[cur] <- bs.[cur] + 1
            go bs (toDistribute - 1) (( cur + 1) % Array.length bs)


    // fuck it, let's use mutable state, but keep it local
    let banks' = Array.ofList banks
    let toDistribute = banks'.[n]
    banks'.[n] <- 0

    go banks' toDistribute ((n + 1) % Array.length banks')
    |> Array.toList

let findDuplicate banks =
    let rec go seen bs =
        let nextCycle = distribute bs (findLargest bs)
        if Set.contains nextCycle seen then (Set.count seen + 1) else go (Set.add nextCycle seen) nextCycle

    go Set.empty banks

let initBanks = (parse input)

printfn "%A" (findDuplicate initBanks)

