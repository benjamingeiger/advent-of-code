// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic 

let readLines filePath = System.IO.File.ReadLines filePath
let input =
    readLines "input.txt"
        |> Seq.head
        |> Seq.toList
        |> List.map (fun c -> (int c) - (int '0'))
        |> fun l -> l @ [((List.max l) + 1) .. 1000000]
        |> List.toArray

let step circle =
    let cup_count = Array.length circle
    let new_circle = Array.zeroCreate (cup_count)
    let current_cup = Array.head circle
    let rec find_predecessor cup =
        if cup = 1 then find_predecessor (cup_count + 1)
        else
            let pred_location = Array.findIndex ((=) (cup - 1)) circle
            if pred_location = 1 || pred_location = 2 || pred_location = 3 then find_predecessor (cup - 1)
            else pred_location

    let pred_location = find_predecessor current_cup

    // removed
    Array.blit circle 1 new_circle (pred_location - 3) 3
    (*printfn "removed %A" new_circle*)
    // before
    Array.blit circle 4 new_circle 0 (pred_location - 3)
    (*printfn "before: %A" new_circle*)
    // after
    Array.blit circle (pred_location + 1) new_circle (pred_location) ((cup_count - pred_location) - 1)
    (*printfn "after: %A" new_circle*)
    // wrapped
    Array.blit circle 0 new_circle (cup_count - 1) 1
    (*printfn "wrapped: %A" new_circle*)

    new_circle

printfn "%A" input
(*printfn "%A" (step input)*)

let rec generate f x count =
    if count = 1 then f x
    else
        let cur = f x
        if count % 10000 = 0 then
            printfn "%d: %A" count cur
        (generate f cur (count - 1))

let result = (generate step input 10000000)
printfn "%A" result

let one_cup = Array.findIndex ((=) 1) result
printfn "%A" ((bigint (result.[one_cup + 1])) * (bigint (result.[one_cup + 2])))
