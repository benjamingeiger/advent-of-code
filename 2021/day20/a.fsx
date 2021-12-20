// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> List.ofSeq

let parseInput input =
    let parts = input |> splitSeq ((=) "")

    let algorithm = Seq.head parts |> Seq.head

    let image =
        parts
        |> Seq.skip 1
        |> Seq.head
        |> indexCharacters ((=) '#')
        |> Map.ofSeq

    (algorithm, image)

let getPixel def image (r, c) =
    match Map.tryFind (r, c) image with
    | None -> def
    | Some false -> 0
    | Some true -> 1

let neighbors (r, c) =
    [
        (r - 1, c - 1); (r - 1, c); (r - 1, c + 1);
        (r,     c - 1); (r,     c); (r,     c + 1);
        (r + 1, c - 1); (r + 1, c); (r + 1, c + 1)
    ]

let enhancePixel def (algorithm : string) image (r, c) =
    (r, c)
    |> neighbors
    |> List.map (getPixel def image)
    |> List.fold (fun acc x -> acc * 2 + x) 0
    |> fun index -> (algorithm.[index]) = '#'

let enhanceImage def algorithm image =
    let pixels = image |> Map.filter (fun _ v -> v) |> Map.toSeq |> Seq.map fst |> Seq.toList

    let minR = pixels |> Seq.minBy fst |> fst |> fun x -> x - 1
    let maxR = pixels |> Seq.maxBy fst |> fst |> fun x -> x + 1
    let minC = pixels |> Seq.minBy snd |> snd |> fun x -> x - 1
    let maxC = pixels |> Seq.maxBy snd |> snd |> fun x -> x + 1

    List.allPairs [minR..maxR] [minC..maxC]
    |> List.map (fun (r, c) -> ((r, c), enhancePixel def algorithm image (r, c)))
    |> Map.ofList

let doubleEnhance algorithm image =
    image
    |> enhanceImage 0 algorithm
    |> enhanceImage 1 algorithm

let dumpImage image =
    let pixels = image |> Map.filter (fun _ v -> v) |> Map.toSeq |> Seq.map fst |> Seq.toList

    let minR = pixels |> Seq.minBy fst |> fst |> fun x -> x - 1
    let maxR = pixels |> Seq.maxBy fst |> fst |> fun x -> x + 1
    let minC = pixels |> Seq.minBy snd |> snd |> fun x -> x - 1
    let maxC = pixels |> Seq.maxBy snd |> snd |> fun x -> x + 1

    [minR..maxR]
    |> List.map (fun row ->
        [minC..maxC]
        |> List.map (fun col -> if (image |> Map.tryFind (row, col) |> Option.defaultValue false) then "#" else ".")
        |> String.concat "")
    |> String.concat "\n"

let countLitPixels image =
    image
    |> Map.filter (fun _ v -> v)
    |> Map.toSeq
    |> Seq.length

let result =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let (algorithm, image) = parseInput input

    let returnValue = (image |> doubleEnhance algorithm |> countLitPixels)

    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds

    returnValue

printfn "%A" result
