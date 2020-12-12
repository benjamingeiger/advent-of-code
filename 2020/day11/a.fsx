// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> List.ofSeq

let numrows = Seq.length input
let numcols = Seq.length input.[0]

let initState =
    [for r in 0 .. (numrows - 1) do yield! [for c in 0 .. (numcols - 1) do yield ((r, c), input.[r].[c])]]

let neighbors (r, c) =
    let ns = [
        (r - 1, c - 1); (r - 1, c); (r - 1, c + 1);
        (r, c - 1); (r, c + 1); 
        (r + 1, c - 1); (r + 1, c); (r + 1, c + 1)]

    List.filter (fun (r, c) -> r >= 0 && r < numrows && c >= 0 && c < numcols) ns

let occupiedNeighbors state (r, c) =
    neighbors (r, c)
    |> List.map (fun (r', c') -> List.find (fun ((r'', c''), v) -> r' = r'' && c' = c'') state)
    |> List.map snd
    |> List.filter ((=) '#')
    |> List.length

let updateSeat state ((r, c), current) =
    let neighbors = occupiedNeighbors state (r, c)

    let next =
        match current with
        | 'L' when neighbors = 0 -> '#'
        | '#' when neighbors >= 4 -> 'L'
        | _ -> current
    ((r, c), next)

let occupied state =
    state
    |> List.filter (fun (_, c) -> c = '#')
    |> List.length

let doStep state =
    let result =
        state
        |> List.map (updateSeat state)

    printfn "%A" (occupied result)

    result


let rec fix f state =
    let state' = f state
    if state' = state then state else fix f state'

printfn "%A" (occupied (fix doStep initState))

