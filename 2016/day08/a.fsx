// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let screenRows = 6
let screenCols = 50

type Instruction = Rect of int * int | RotateColumn of int * int | RotateRow of int * int

let parseInstruction (s : string) =
    if (s.StartsWith "rect") then
        let rectRegex = @"rect (\d+)x(\d+)"
        let m = Regex.Match(s, rectRegex)
        let cols = int m.Groups.[1].Value
        let rows = int m.Groups.[2].Value

        Rect (rows, cols)

    else if (s.StartsWith "rotate column") then
        let rotcolRegex = @"rotate column x=(\d+) by (\d+)"
        let m = Regex.Match(s, rotcolRegex)
        let col = int m.Groups.[1].Value
        let amount = int m.Groups.[2].Value

        RotateColumn (col, amount)

    else if (s.StartsWith "rotate row") then
        let rotrowRegex = @"rotate row y=(\d+) by (\d+)"
        let m = Regex.Match(s, rotrowRegex)
        let row = int m.Groups.[1].Value
        let amount = int m.Groups.[2].Value

        RotateRow (row, amount)
    else failwith (sprintf "invalid command %s" s)

let instructions =
    input
    |> Seq.map parseInstruction

printfn "%A" instructions

let screen = Array2D.create screenRows screenCols false
let screenOn = Array2D.create screenRows screenCols true

let updateScreen screen inst =
    let rotateList n l =
        let rotateAmount = (List.length l) - n
        (List.skip rotateAmount l) @ (List.take rotateAmount l)
    let blit = Array2D.blit screenOn 0 0 screen 0 0
    let getColumn c = screen.[*,c..c] |> Seq.cast<bool> |> Seq.toList
    let setColumn c l =
        (*printfn "setColumn %A %A" c (Seq.length l)*)
        let tmp = Array2D.init (List.length l) 1 (fun r c -> l.[r])
        Array2D.blit tmp 0 0 screen 0 c (List.length l) 1
    let rotateColumn c n =
        getColumn c
        |> rotateList n
        |> setColumn c
    let getRow r = screen.[r..r,*] |> Seq.cast<bool> |> Seq.toList
    let setRow r l = 
        (*printfn "setRow %A %A" r (Seq.length l)*)
        let tmp = Array2D.init 1 (List.length l) (fun r c -> l.[c])
        Array2D.blit tmp 0 0 screen r 0 1 (List.length l)
    let rotateRow r n =
        getRow r
        |> rotateList n
        |> setRow r

    match inst with
    | Rect (rs, cs) -> blit rs cs
    | RotateRow (row, amount) -> rotateRow row amount
    | RotateColumn (column, amount) -> rotateColumn column amount

    screen

let result = Seq.fold updateScreen screen instructions

printfn "%A" result

let countPixels screen =
    screen
    |> Seq.cast<bool>
    |> Seq.filter id
    |> Seq.length

printfn "%A" (countPixels result)
