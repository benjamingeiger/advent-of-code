// vim: set et ts=4 sw=4 list :

open System

let input = 293801

type State = int * int list * int * int

let initState = (2, [7; 3], 0, 1)

let tick (numRecipes, recipes, elf1, elf2) =
    let getRecipe n = List.item (numRecipes - (n + 1)) recipes

    let (recipe1, recipe2) = (getRecipe elf1, getRecipe elf2)

    let newRecipe1 = (recipe1 + recipe2) / 10
    let newRecipe2 = (recipe1 + recipe2) % 10

    let numRecipes' = if newRecipe1 > 0 then numRecipes + 2 else numRecipes + 1
    let recipes' = if newRecipe1 > 0 then newRecipe2 :: newRecipe1 :: recipes else newRecipe2 :: recipes

    let elf1' = (elf1 + recipe1 + 1) % numRecipes'
    let elf2' = (elf2 + recipe2 + 1) % numRecipes'

    (numRecipes', recipes', elf1', elf2')

let rec run offset = function
    | (numRecipes, recipes, _, _) as state when numRecipes >= offset + 10 ->
        recipes
        |> List.take (numRecipes - offset)
        |> List.rev
        |> List.take 10
    | state -> run offset (tick state)

printfn "%A" (run input initState)
