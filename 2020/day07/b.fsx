// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

let contains x = Seq.exists ((=) x)

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.toList

let bagPattern = @"(.*) bags? contain (.*)\."
let bagRegex = Regex bagPattern

let parseLine s =
    let m = bagRegex.Match s
    let outerBag = Regex.Replace(m.Groups.[1].Value, @" bags?$", "")
    let innerBags = 
        let parseInnerBag b =
            match b with
            | "no other" -> [(0, "no other")]
            | xs -> 
                xs
                |> fun b -> b.Split(" ", 2)
                |> fun xs -> [(int xs.[0], xs.[1])]

        (m.Groups.[2].Value.Split ", ")
            |> Seq.map (fun s -> Regex.Replace(s, @" bags?$", ""))
            |> Seq.collect parseInnerBag

    let rec cross first second =
        match second with
        | [] -> []
        | (n, h) :: t -> (first, h, n) :: (cross first t)

    cross outerBag (Seq.toList innerBags)

let bagRules =
    input 
    |> Seq.collect parseLine

let rec countBags rules start =
    let contents bag =
        rules
        |> Seq.filter (fun (x, y, n) -> x = bag && n > 0)
        |> Seq.map (fun (x, y, n) -> (y, n))

    let result =
        contents start
        |> Seq.map (fun (next, count) -> count * (countBags rules next))
        |> Seq.fold (+) 1

    result

// minus 1 because we don't want to count the outermost shiny gold bag.
let result = (countBags bagRules "shiny gold") - 1
printfn "%A" result
