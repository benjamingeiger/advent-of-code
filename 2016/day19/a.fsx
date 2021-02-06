// vim: set et ts=4 sw=4 list :

open System

let input = 3014603

// https://cp-algorithms.com/others/josephus_problem.html
(*let rec josephus n k =*)
    (*printfn "%A %A" n k*)
    (*if n = 1 then 1*)
    (*else ((josephus (n - 1) k) + k - 1) % n + 1*)

// gotta do this one bottom-up because the above overflows the stack.
let josephus n k =
    let rec step n' prev =
        if n' > n then prev
        else
            let newPrev = ((prev + k - 1) % n') + 1
            step (n' + 1) newPrev

    step 2 1

printfn "%A" (josephus input 2)
