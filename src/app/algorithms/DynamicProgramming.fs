module DynamicProgramming

open System
open Hacker

// Fibonacci Modified
let fibonacciModified () =
    let data = readIntArray ()
    let f1, f2 = bigint data.[0], bigint data.[1]
    let n = data.[2]

    let rec fibs a b = seq {
        let current = a + b * b
        yield current
        yield! fibs b current
    }

    Console.WriteLine (fibs f1 f2 |> Seq.skip (n-3) |> Seq.head)

// [|"2 2 20"|] |> hacker fibonacciModified


// Maximum Subarray
let maximumSubArray () =
    let cases = readInt ()
    for i in 1..cases do
        readInt () |> ignore
        let array = readIntArray ()

        let _, _, _, bestContSum, startIndex, endIndex, bestSum =
            array
            |> Seq.fold (
                fun ((cs, csi, ci, bcs, bsi, bei, bs) as res) i ->
                    let temp = cs + i
                    let newCsi = if temp > 0 && cs = 0 then ci else csi
                    let newCs = if temp > 0 then temp else 0
                    let newCi = csi + 1
                    let newBcs, newBsi, newBei =
                        if newCs > bcs
                        then newCs, newCsi, newCi
                        else bcs, bsi, bei
                    let newBs = bs + if i>0 then i else 0
                    newCs, newCsi, newCi, newBcs, newBsi, newBei, newBs
                ) (0, -1, -1, 0, -1, -1, 0)
        printfn "%d %d" bestContSum bestSum
