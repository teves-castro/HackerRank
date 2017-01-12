module Recursion

open System
open Hacker

// Pascal's triangle
let pascal =
    let cl x xs = seq { 
        yield x
        yield! xs
    }
    let cr xs x = seq { 
        yield! xs
        yield x
    }

    let rec pascal' previous = seq {
        yield previous
        let next = (0UL |>cl<| previous) |> Seq.zip <| (previous |>cr<| 0UL) |> Seq.map (fun (x,y) -> x+y)
        yield! pascal' next
    }
    pascal' [1UL]

let pascal1 n = 
    let rec nextrow row y ys =
        match row with
        | [] -> 1UL::ys
        | x::xs -> nextrow xs x ((x + y)::ys)
    and addrow i row rows = 
        if i <= 1UL then row::rows
        else addrow (i-1UL) (nextrow row 0UL []) (row::rows)
    addrow n [1UL] [] 

let sierpinski l = seq {
    for y in [0..31] do
        for x in [0..62] do
            yield '1'
        yield '\n'
}
