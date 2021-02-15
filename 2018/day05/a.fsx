// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.head

let step remaining current =
    let doesMatch c1 c2 =
        Char.ToUpper(c1) = Char.ToUpper(c2)
            && (Char.IsUpper(c1) && Char.IsLower(c2)
                || Char.IsLower(c1) && Char.IsUpper(c2))

    match remaining with
    | [] -> [current]
    | c :: rest -> if doesMatch c current then rest else (current :: remaining)

printfn "%A" (List.length (Seq.fold step [] input))
