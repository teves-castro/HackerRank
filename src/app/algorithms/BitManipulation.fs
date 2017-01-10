module BitManipulation

open System
open Hacker

let seqXOR ls = ls |> Seq.fold (^^^) 0

// Sansa and XOR
let sansaAndXOR () =
    let solve a =
        let len = a |> Seq.length
        if len % 2 = 0 then 
            0
        else
            a 
            |> Seq.zip [ 1 .. len ] 
            |> Seq.filter (fun (i, v) -> i % 2 <> 0) 
            |> Seq.map snd 
            |> seqXOR

    let cases = readInt ()
    [1..cases]
    |> Seq.map (readInt >> ignore >> readIntArray >> solve)
    |> Seq.iter (sprintf "%d" >> writeLine)


// Maximizing XOR
let maximizeXOR () =
    let l, r = readInt (), readInt ()
    seq { for a in [ l..r ] do for b in [ a..r ] -> a , b }
    |> Seq.map (fun (a,b) -> a ^^^ b) |> Seq.toList
    |> Seq.max
    |> writeLine

// [|
//     "10";
//     "15";
// |] |> hacker maximizeXOR


// Counter game
let counterGame () =
    let powers = seq { for i in [64 .. -1 .. 0] -> pown 2I i } |> List.ofSeq
    let numberOfCases = readInt()

    let solve n =
        n 
        |> Seq.unfold (
            fun s -> 
                match s with
                | v when v = 1I -> None 
                | v when v.IsPowerOfTwo -> Some(v, v / 2I)
                | v -> 
                    let lesserPower = powers |> List.filter (fun p -> p < v) |> List.tryHead
                    match lesserPower with
                    | None -> None
                    | Some(p) -> Some(v, v - p))
        |> Seq.length

    [1..numberOfCases]
    |> List.map (readBigint >> solve >> (fun t -> if t % 2 = 0 then "Richard" else "Louise")) 
    |> List.iter (sprintf "%s" >> writeLine)
