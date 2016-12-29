module Greedy

open System
open Hacker

// Sherlock and The Beast
let sherlockAndTheBeast () =
    let solve n =
        let maxDecent =
            seq { for i in 0 .. 5 .. n -> n-i, i }
            |> Seq.filter (fun (f, t) -> f % 3 = 0)
            |> Seq.toList
            |> tryHead
        match maxDecent with
        | None -> "-1"
        | Some (f, t) -> "".PadLeft(f, '5') + "".PadLeft(t, '3')

    let cases = readInt()
    for i in 1..cases do
        (readInt >> solve >> Console.WriteLine) ()

// [| "20"; "26786"; "28643"; "62205"; "99775"; "42471"; "51463"; "4296"; "80994"; "57669"; "72134"; "25907"; "19949"; "72832"; "99631"; "90974"; "38777"; "59798"; "15508"; "57254"; "60757" |]
// [| "20"; "98765"; "98765"; "98765"; "98765"; "98765"; "98765"; "98765"; "98765"; "98765"; "98765"; "98765"; "98765"; "98765"; "98765"; "98765"; "98765"; "98765"; "98765"; "98765"; "98765" |]
// |> hacker sherlockAndTheBeast
