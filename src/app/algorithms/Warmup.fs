module Warmup

open System
open Hacker

// Compare the triplets
let compareTriplets () =
    let aa = readIntArray ()
    let bb = readIntArray ()
    let sa, sb = 
        bb 
        |> Seq.zip aa
        |> Seq.fold (fun (sa,sb) r -> 
                        match r with
                        | a, b when a > b -> sa+1, sb
                        | a, b when a < b -> sa, sb+1
                        | _ -> sa, sb) (0,0)
    printfn "%d %d" sa sb

// Simple Array Sum
let simpleArraySum() =
    readLine() |> ignore
    readIntArray() |> Array.sum |> Console.WriteLine

// [|
//  "6";
//  "1 2 3 4 10 11"
// |] |> hacker simpleArraySum

// Diagonal Difference
let diagonalDifference() =
    let size, matrix = readIntSquareMatrix()
    let ubound = size - 1

    let fstDiag, sndDiag =
        [ for i in 0..ubound -> matrix.[i].[i], matrix.[ubound - i].[i] ]
        |> split
        ||> (Seq.sum)

    let diff = Math.Abs(fstDiag - sndDiag)
    printfn "%d" diff
// [|
//  "3";
//  "11 2 4";
//  "4 5 6";
//  "10 8 -12"
// |] |> hacker diagonalDifference


// Plus Minus
let plusMinus() =
    let count = readInt() |> float
    let numbers = readIntArray()

    let positive =
        numbers
        |> Seq.filter ((<) 0)
        |> Seq.length
        |> float

    let negative =
        numbers
        |> Seq.filter ((>) 0)
        |> Seq.length
        |> float

    let zero =
        numbers
        |> Seq.filter ((=) 0)
        |> Seq.length
        |> float

    printfn "%.3f\n%.3f\n%.3f\n" (positive / count) (negative / count) (zero / count)

// [|
//  "6";
//  "-4 3 -9 0 4 1"
// |] |> hacker plusMinus


// Staircase
let staircase() =
    let height = readInt() - 1
    for i in 0..height do
        for j in i + 1..height do
            printf " "
        for j in 0..i do
            printf "#"
        printfn ""

// [| "6" |] |> hacker staircase


// Time Conversion
let timeConversion() =
    let time = readLine()
    printfn "%s" (DateTime.Parse(time).ToString("HH:mm:ss"))

// [| "07:05:45PM" |] |> hacker timeConversion
