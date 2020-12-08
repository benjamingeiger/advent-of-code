// vim: set et ts=4 sw=4 list :

open System

let contains x = Seq.exists ((=) x)

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"



