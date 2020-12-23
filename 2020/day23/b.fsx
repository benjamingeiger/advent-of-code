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

let maxCup = input |> List.max

let cups =
    (List.last input) :: input
        |> List.pairwise
        |> Map.ofList

let nextCup circle cup = Map.find cup circle

let cupsToList cups =
    let rec go start cur lst =
        let next = nextCup cups cur
        if next = start then lst
        else go start next (next :: lst)

    List.rev (go 1 1 [1])

(*printfn "%A" (cupsToList cups)*)

let step (currentCup, initCircle) =
    let removeCups circle =
        let firstRemoved = currentCup |> nextCup circle
        let secondRemoved = firstRemoved |> nextCup circle
        let thirdRemoved = secondRemoved |> nextCup circle
        let newNext = thirdRemoved |> nextCup circle

        let newCircle =
            circle
            |> Map.add currentCup newNext
            |> Map.add thirdRemoved firstRemoved

        ([firstRemoved; secondRemoved; thirdRemoved], newCircle)

    let rec predecessor circle removed cup =
        if cup = 1 then predecessor circle removed (maxCup + 1)
        else if (List.contains (cup - 1) removed) then predecessor circle removed (cup - 1)
        else cup - 1

    let insertCups destination firstRemoved circle =
        let prevNext = nextCup circle destination
        let lastRemoved = firstRemoved |> nextCup circle |> nextCup circle

        circle
        |> Map.add destination firstRemoved
        |> Map.add lastRemoved prevNext

    let (removed, circle') = removeCups initCircle
    let prev = predecessor circle' removed currentCup
    let newCircle = insertCups prev (List.head removed) circle'
    let newCurrentCup = nextCup newCircle currentCup

    (newCurrentCup, newCircle)

let initialCup = List.head input

let rec generate f state count =
    if count % 100000 = 0 then printfn "%d %A" count state

    if count = 0 then state
    else generate f (f state) (count - 1)

let (finalCup, finalCircle) = generate step (initialCup, cups) 10000000

let firstResult = nextCup finalCircle 1
let secondResult = nextCup finalCircle firstResult

printfn "%A" ((bigint firstResult) * (bigint secondResult))
