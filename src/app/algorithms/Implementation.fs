module Implementation

open System
open Hacker

let matrixLayerRotation() =
    // let m, n, rotations = readInt (), readInt (), readInt ()
    let m,n,r = 5,4,10
    let matrix = readIntMatrix m

    let ms = [m .. -2 .. 1]
    let ns = [n .. -2 .. 1]

    let sizes = 
        ms |> Seq.zip <| ns 
        |> Seq.map 
            (( fun (m,n) -> m, n, (max m n) * 2 + (min m n) * 2 - 4 ) >>
             ( fun (m,n,p) -> m, n, r % p ))
        |> Seq.toList

    let matrix =
        [|
            [|1;2;3|];
            [|3;4;5|]
        |]

    let column c (matrix: int[][]) =
        matrix |> Seq.map (fun row -> row.[c]) |> Seq.toList

    let row r (matrix: int[][]) =
        matrix.[r] |> Seq.toList
    
    let unwind l (r,c) matrix =
        (matrix |> column l) 
        @ (matrix |> row (r - l - 1))
        @ (matrix |> column (c - l - 1) |> List.rev) 
        @ (matrix |> row l |> List.rev)

    // TODO: let wind l (r,c) matrix =

    let rec rotate matrix (m,n,r) l =
        // TODO shift level l of the matrix r positions anti-clockwise
        matrix

    unwind 0 (m,n) matrix