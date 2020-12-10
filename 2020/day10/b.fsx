// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic 

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.map int |> Seq.toList |> (fun xs -> 0 :: xs)

let target = (List.max input) + 3

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

let rec pathsTo =
    let pathsTo' target = 
        let hopsDown = List.filter (fun n -> n < target && target - n <= 3) input

        (*printfn "%A %A" target hopsDown*)

        if target = 0
        then 1I
        else List.reduce (+) (List.map pathsTo hopsDown)

    memoize pathsTo'

printfn "%A" (pathsTo target)

    
