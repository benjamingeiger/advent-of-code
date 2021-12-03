// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> Seq.cache

let mostCommonBit pos (strs : seq<string>) =
    strs
    |> Seq.map (fun s -> s.[pos])
    |> Seq.countBy id
    |> Seq.maxBy (fun (x, y) -> (y, x))
    |> fst

let binToInt (s : string) =
    let step total digit =
        match digit with
        | '0' -> total * 2
        | '1' -> total * 2 + 1
        | _ -> failwith "foo"

    Seq.fold step 0 s

let findOxygenRating strs =
    let rec step pos strs' =
        if (Seq.length strs' = 1)
        then binToInt (Seq.head strs')
        else
            let mcb = mostCommonBit pos strs'
            let strs'' =
                strs'
                |> Seq.filter (fun (s : string) -> s.[pos] = mcb)

            step (pos + 1) strs''

    step 0 strs

let findCO2Rating strs =
    let rec step pos strs' =
        if (Seq.length strs' = 1)
        then binToInt (Seq.head strs')
        else
            let mcb = if mostCommonBit pos strs' = '1' then '0' else '1'
            let strs'' =
                strs'
                |> Seq.filter (fun (s : string) -> s.[pos] = mcb)

            step (pos + 1) strs''

    step 0 strs

let oxygenRating = findOxygenRating input
let co2Rating = findCO2Rating input

printfn "%A" (oxygenRating * co2Rating)
