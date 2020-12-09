// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.map bigint.Parse

// found in part 1
let target = 1038347917I

let result = 
    // windows of at least 2 numbers
    [2..(Seq.length input)]
    // get all consecutive ranges of numbers, shortest first
    |> Seq.collect (fun n -> Seq.windowed n input)
    // remove the ones that don't sum to the target
    |> Seq.filter (fun xs -> (Seq.reduce (+) xs) = target)
    // take the first one out of the seq
    // ideally this will stop the calculation once we find one
    |> Seq.head
    // get the sum of the largest and smallest
    |> (fun xs -> (Array.max xs) + (Array.min xs))

printfn "%A" result
