module Advanced

open System
open Common
open Microsoft.FSharp.Collections
open Hacker

let jimAndTheSkyscrapers xs = 
  let len = Array.length xs
  let indexes = [| 0 .. len - 1 |]
  indexes |> allPairs indexes 
  |> Seq.filter (fun (i,j) -> i <> j && xs.[i]=xs.[j]) 
  |> Seq.filter (fun (i,j) -> 
                  let l,h = min i j, max i j
                  xs.[l+1..h-1] |> Seq.exists (fun h -> h > xs.[i]) |> not)

let solveJim () =
    readInt () |> ignore
    let hs = readIntArray ()
    sprintf "%d" (hs |> jimAndTheSkyscrapers |> Seq.length) |> write
[|
 "6";
 "1 1000 1"
|] |> hacker solveJim
