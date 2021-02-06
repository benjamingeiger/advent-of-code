// vim: set et ts=4 sw=4 list :

open System

let input = 3014603

// Okay, I'm not entirely certain how or why this works, but there
// is a pattern with a growing input, and it's all based on powers of 3.
//
// Powers of 3 are themselves: in a circle of 9 (3^2) elves, elf #9 gets
// all of the presents.
//
// After that, it loops forward by one elf:
//      10: 1
//      11: 2
//      12: 3
//      ... and so on... until it reaches 18 (2 * 3^2).
//      18: 9
//      19: 11
//      20: 13
//      ... until it reaches 27 (3^3).
//      25: 23
//      26: 25
//      27: 27

let lowerPowerOf3 =
    let rec go r = if 3 * r > input then r else go (3 * r)
    go 1

let offset =
    if (input - lowerPowerOf3) <= lowerPowerOf3 then
        (input - lowerPowerOf3)
    else
        lowerPowerOf3 + (input - lowerPowerOf3) * 2

printfn "%A" offset
