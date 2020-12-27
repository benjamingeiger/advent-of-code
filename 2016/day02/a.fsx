// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let processStep position direction =
    let moveUp = function
        | 4 -> 1 | 7 -> 4
        | 5 -> 2 | 8 -> 5
        | 6 -> 3 | 9 -> 6
        | x -> x

    let moveDown = function
        | 1 -> 4 | 4 -> 7
        | 2 -> 5 | 5 -> 8
        | 3 -> 6 | 6 -> 9
        | x -> x

    let moveLeft = function
        | 2 -> 1 | 3 -> 2
        | 5 -> 4 | 6 -> 5
        | 8 -> 7 | 9 -> 8
        | x -> x

    let moveRight = function
        | 1 -> 2 | 2 -> 3
        | 4 -> 5 | 5 -> 6
        | 7 -> 8 | 8 -> 9
        | x -> x

    match direction with 
    | 'U' -> moveUp position
    | 'D' -> moveDown position
    | 'L' -> moveLeft position
    | 'R' -> moveRight position
    | x -> failwith (sprintf "invalid direction %c" direction)

let processLine startingPosition directions =
    Seq.fold processStep startingPosition directions

let result = Seq.tail (Seq.scan processLine 5 input)

printfn "%A" (Seq.toList result)
