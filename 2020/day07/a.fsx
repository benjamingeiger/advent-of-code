// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.toList

let bagPattern = @"(.*) bags? contain (.*)\."
let bagRegex = Regex bagPattern

let rec cross first second =
    match second with
    | [] -> []
    | h :: t -> (first, h) :: (cross first t)

let parseLine s =
    let m = bagRegex.Match s
    let outerBag = Regex.Replace(m.Groups.[1].Value, @" bags?$", "")
    let innerBags = 
        m.Groups.[2].Value.Split ", "
        |> Array.toList
        |> List.map (fun b -> Regex.Replace(b, @"^\d+ ", ""))
        |> List.map (fun b -> Regex.Replace(b, @" bags?$", ""))

    match innerBags with
    | ["no other bags"] -> []
    | _ -> cross outerBag innerBags

let bagRules =
    input 
    |> Seq.collect parseLine
    |> Seq.map (fun (x, y) -> (y, x))

let contains x = Seq.exists ((=) x)

let rec dfs edges start =
    let neighbors n =
        edges
        |> Seq.filter (fun (x, y) -> x = n)
        |> Seq.map snd

    let rec dfs' current visited =
        let ns =
            neighbors current
            |> Seq.filter (fun n -> not (contains n visited))
        let foldFunc v n =
            if (contains n v) then v else dfs' n v
        Seq.fold foldFunc (current :: visited) ns

    dfs' start []

let outermostBags = dfs bagRules "shiny gold" |> Seq.sort |> Seq.distinct

printfn "%A" outermostBags
// -1 because the shiny gold bag itself can't be on the outside
printfn "%A" ((Seq.length outermostBags) - 1)
