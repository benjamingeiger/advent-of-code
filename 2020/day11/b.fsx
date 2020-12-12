// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> List.ofSeq

let numrows = Seq.length input
let numcols = Seq.length input.[0]

let initState = Array2D.init numrows numcols (fun r c -> input.[r].[c])

(*printfn "%A" initState*)

let isValidIndex (r, c) = r >= 0 && r < numrows && c >= 0 && c < numcols

let getValue (state : char [,]) (r, c) =
    state.[r,c]

let scan dr dc =
    let rec scan' state (r, c) =
        if (not (isValidIndex (r + dr, c + dc)))
        then 'L'
        else
            match (getValue state (r + dr, c + dc)) with
            | '.' -> scan' state (r + dr, c + dc)
            | v -> v

    scan'

let neighborScan = [scan -1 0; scan -1 1; scan 0 1; scan 1 1; scan 1 0; scan 1 -1; scan 0 -1; scan -1 -1]

let occupiedNeighbors state (r, c) =
    let neighbors = 
        neighborScan
        |> List.map (fun f -> f state (r, c))

    neighbors
    |> List.filter ((=) '#')
    |> List.length

let updateSeat state r c =
    let neighbors = occupiedNeighbors state (r, c)
    let current = getValue state (r, c)

    let next =
        match current with
        | 'L' when neighbors = 0 -> '#'
        | '#' when neighbors >= 5 -> 'L'
        | _ -> current
    next

let occupied state =
    state
    |> Seq.cast<char>
    |> Seq.toList
    |> List.filter (fun c -> c = '#')
    |> List.length

let doStep state =
    let newState = Array2D.init numrows numcols (updateSeat state)

    newState

let rec fix f state =
    let state' = f state
    if state' = state then state else fix f state'

(*let foo = doStep (doStep initState)*)
(*printfn "%A" foo*)
(*[>printfn "%A" (occupied foo)<]*)
printfn "%A" (occupied (fix doStep initState))

