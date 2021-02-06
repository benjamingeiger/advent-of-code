// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

type Tile = Safe | Trap

let parseInputLine s =
    s
    |> Seq.map (function '.' -> Safe | '^' -> Trap)
    |> Seq.toList

let wrap row = List.concat [[Safe]; row; [Safe]]

let isTrap = function
    | Trap :: _ :: Safe :: _ -> Trap
    | Safe :: _ :: Trap :: _ -> Trap
    | _ -> Safe

let getNext row =
    wrap row
    |> List.windowed 3
    |> List.map isTrap

let countSafe r = r |> List.filter (function | Safe -> true | Trap -> false) |> List.length

let initRow = parseInputLine (Seq.head input)

(*printfn "%A" initRow*)
(*printfn "%A" (getNext initRow)*)

let results =
    initRow
    |> Seq.unfold (fun (st : Tile list) -> Some (countSafe st, getNext st))
    |> Seq.take 400000
    |> Seq.fold (+) 0

printfn "%A" results
