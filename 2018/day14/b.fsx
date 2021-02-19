// vim: set et ts=4 sw=4 list :

open System

let input = 293801

type State = int * int list * int * int

let initState = (2, (Map.ofList [(0, 3); (1, 7)]), 0, 1)

let tick (numRecipes, recipes, elf1, elf2) =
    let getRecipe n = Map.find n recipes

    let (recipe1, recipe2) = (getRecipe elf1, getRecipe elf2)

    let newRecipe1 = (recipe1 + recipe2) / 10
    let newRecipe2 = (recipe1 + recipe2) % 10

    let numRecipes' = if newRecipe1 > 0 then numRecipes + 2 else numRecipes + 1
    let recipes' =
        if newRecipe1 > 0 
        then (Map.add numRecipes newRecipe1 (Map.add (numRecipes + 1) newRecipe2 recipes))
        else (Map.add numRecipes newRecipe2 recipes)

    let elf1' = (elf1 + recipe1 + 1) % numRecipes'
    let elf2' = (elf2 + recipe2 + 1) % numRecipes'

    (numRecipes', recipes', elf1', elf2')

let run searchPattern state =
    let searchPattern' =
        searchPattern
        |> sprintf "%d"
        |> Seq.map (fun c -> int c - int '0')
        |> List.ofSeq
        |> List.rev

    let matchSeqs s1 s2 =
        Seq.zip s1 s2
        |> Seq.exists (fun (a, b) -> a <> b)
        |> not

    let recentRecipes numRecipes recipes =
        [(numRecipes - 8)..(numRecipes - 1)]
        |> List.rev
        |> List.map (fun n -> Map.find n recipes)

    let rec run' state' =
        let (numRecipes, recipes, _, _) = state'
        if numRecipes < 10 then run' (tick state')
        else
            let recents = recentRecipes numRecipes recipes
            if matchSeqs searchPattern' recents
            then 
                printfn "%A" recents
                numRecipes - 6
            elif matchSeqs searchPattern' (List.tail recents)
            then
                printfn "%A" recents
                numRecipes - 7
            else run' (tick state')

    run' state

printfn "%A" (run input initState)


