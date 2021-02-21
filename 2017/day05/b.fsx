// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.map int

let failIfNone error f x =
    match f x with
    | None -> failwith error
    | Some x' -> x'

module ZipperList =
    type ZipperList<'a> = Empty | ZipperList of 'a list * 'a * 'a list

    let empty = Empty

    let ofList l = ZipperList ([], List.head l, List.tail l)

    let ofSeq s = s |> Seq.toList |> ofList

    let current = function
        | Empty -> failwith "Empty list, no current element"
        | ZipperList (_, cur, _) -> cur

    let updateCurrent e = function
        | Empty -> failwith "Attempted to update nonexistent element"
        | ZipperList (front, cur, back) -> ZipperList (front, e, back)

    let tryShiftForward = function
        | Empty -> None
        | ZipperList (_, _, []) -> None
        | ZipperList (front, cur, back) -> Some <| ZipperList (cur :: front, List.head back, List.tail back)

    let shiftForward zl = failIfNone "Attempted to shift past end of list" tryShiftForward zl

    let tryShiftBackward = function
        | Empty -> None
        | ZipperList ([], _, _) -> None
        | ZipperList (front, cur, back) -> Some <| ZipperList (List.tail front, List.head front, cur :: back)

    let shiftBackward zl = failIfNone "Attempted to shift past beginning of list" tryShiftBackward zl

    let rec tryShiftBy n = function
        | Empty -> None
        | ZipperList _ as zl ->
            if n = 0 then Some zl
            elif n < 0 then Option.bind (fun zl' -> tryShiftBy (n + 1) zl') (tryShiftBackward zl)
            else Option.bind (fun zl' -> tryShiftBy (n - 1) zl') (tryShiftForward zl)

    let shiftBy n = failIfNone "Attempted to shift outside list" (tryShiftBy n)

let jumps = ZipperList.ofSeq input

let rec execute step jumps =
    let cur = ZipperList.current jumps
    let jumps' = ZipperList.updateCurrent (if cur >= 3 then cur - 1 else cur + 1) jumps
    
    match ZipperList.tryShiftBy cur jumps' with
    | None -> step
    | Some jumps'' -> execute (step + 1) jumps''

printfn "%A" (execute 1 jumps)
    
