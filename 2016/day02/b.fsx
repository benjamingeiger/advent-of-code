// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let processStep position direction =
    let moveUp = function
        | 'A' -> '6' | '6' -> '2'
        | 'D' -> 'B' | 'B' -> '7' | '7' -> '3' | '3' -> '1'
        | 'C' -> '8' | '8' -> '4'
        | x -> x

    let moveDown = function
        | '2' -> '6' | '6' -> 'A'
        | '1' -> '3' | '3' -> '7' | '7' -> 'B' | 'B' -> 'D'
        | '4' -> '8' | '8' -> 'C'
        | x -> x

    let moveLeft = function
        | '4' -> '3' | '3' -> '2'
        | '9' -> '8' | '8' -> '7' | '7' -> '6' | '6' -> '5'
        | 'C' -> 'B' | 'B' -> 'A'
        | x -> x

    let moveRight = function
        | '2' -> '3' | '3' -> '4'
        | '5' -> '6' | '6' -> '7' | '7' -> '8' | '8' -> '9'
        | 'A' -> 'B' | 'B' -> 'C'
        | x -> x

    match direction with 
    | 'U' -> moveUp position
    | 'D' -> moveDown position
    | 'L' -> moveLeft position
    | 'R' -> moveRight position
    | x -> failwith (sprintf "invalid direction %c" direction)

let processLine startingPosition directions =
    Seq.fold processStep startingPosition directions

let result = Seq.tail (Seq.scan processLine '5' input)

printfn "%A" (Seq.toList result)
