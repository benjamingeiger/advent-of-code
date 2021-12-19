// vim: set et ts=4 sw=4 list :

open System

let aboutX (x, y, z) = (x, z, -y)
let aboutY (x, y, z) = (-z, y, x)
let aboutZ (x, y, z) = (y, -x, z)

let zeroRotations = [("id", id)]
let oneRotation = [("x", aboutX); ("y", aboutY); ("z", aboutZ)]

let addRotation init =
    List.allPairs init oneRotation |> List.map (fun ((name1, func1), (name2, func2)) -> (sprintf "%s%s" name1 name2, func1 >> func2))

let twoRotations = addRotation oneRotation
let threeRotations = addRotation twoRotations
let fourRotations = addRotation threeRotations
let fiveRotations = addRotation fourRotations
let sixRotations = addRotation fiveRotations

let allRotationOptions = zeroRotations @ oneRotation @ twoRotations @ threeRotations @ fourRotations @ fiveRotations @ sixRotations

let allOutcomes = List.map (fun (n, f) -> (n, f (1, 2, 3))) allRotationOptions

printfn "%A" (allOutcomes |> List.distinctBy snd)
