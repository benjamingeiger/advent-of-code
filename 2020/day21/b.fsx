// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic
open System.Text.RegularExpressions

let enumerateSeq s = Seq.zip (Seq.initInfinite id) s

let memoize (f : 'a -> 'b) =
    let dict = new Dictionary<'a, 'b>()

    let memoizedFunc (input : 'a) =
        match dict.TryGetValue(input) with
        | true, x -> x
        | false, _ ->
            let answer = f input
            dict.Add(input, answer)
            answer

    memoizedFunc

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let foodRegex = Regex @"(.*) \(contains (.*)\)"
let parseFoodItem s =
    let foodMatch = foodRegex.Match s
    if not foodMatch.Success then None
    else
        let ingredients = Set.ofArray ((foodMatch.Groups.[1].Value).Split " ")
        let allergens = Set.ofArray ((foodMatch.Groups.[2].Value).Split ", ")

        Some (ingredients, allergens)

let data =
    input
        |> Seq.map parseFoodItem
        |> Seq.fold
            (fun a x ->
                match x with
                | None -> a
                | Some x' -> x' :: a) []
        |> Seq.cache

let allAllergens =
    data
    |> Seq.map snd
    |> Set.unionMany

let allIngredients =
    data
    |> Seq.map fst
    |> Set.unionMany

let findFoodsWithAllergen data allergen =
    data
    |> Seq.filter (fun (i, a) -> Set.contains allergen a)
    |> Seq.map fst
    |> (fun is -> if Seq.isEmpty is then Set.empty else Set.intersectMany is)

let allProblematicIngredients =
    allAllergens
    |> Seq.map (fun a -> (a, (findFoodsWithAllergen data a)))

let findAllergensForIngredients ingredientMatches =
    let ims = Map.ofSeq ingredientMatches

    let findFirstUnambiguous toMatch =
        let unambiguous =
            toMatch
            |> Map.filter (fun a is -> ((Set.count is) = 1))
            |> Map.toList

        match unambiguous with
        | [] -> None
        | (a, is) :: t -> Some (a, List.exactlyOne (Set.toList is))

    let removePair allergen ingredient toMatch =
        toMatch
        |> Map.remove allergen
        |> Map.map (fun a is -> Set.remove ingredient is)

    let rec go matched toMatch =
        if Map.isEmpty toMatch then matched
        else
            match findFirstUnambiguous toMatch with
            | None -> failwith "nope"
            | Some (a, i) -> go (Map.add a i matched) (removePair a i toMatch)

    go Map.empty ims

let results = findAllergensForIngredients allProblematicIngredients

results
|> Map.toList
|> List.sortBy fst
|> List.map snd
|> String.concat ","
|> printfn "%s"
