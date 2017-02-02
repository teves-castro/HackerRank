module Arrays.Tests

open System
open Expecto
open Expecto.Impl
open FsCheck
open FsCheck.Arb
open FsCheck.Random
open ExpectoFsCheck
open Hacker
open Advanced

let config = { 
  FsCheck.Config.Default with 
    EveryShrink = fun xs -> 
      let xs' = xs.Head :?> uint64 array |> Array.map int
      sprintf "%A\n==> %A\n" xs' (jimAndTheSkyscrapers xs') 
}

[<Tests>]
let arrayProps =
  ftestList "Jim and the Skyscrapers" [

    testPropertyWithConfig config "Jim" <| fun (hs: uint64 array) ->
      let len = hs.Length
      ((len > 1) && (hs |> Array.forall (fun h -> h > 0UL))) ==>
      let valid (i:int,j:int) = 
        let l, h = min i j, max i j
        let hi, hj = hs.[i], hs.[j]
        i<>j && i>=0 && i<len && j>=0 && j<len && hi = hj && (hs.[l+1..h-1] |> Array.exists (fun h -> h > hi) |> not)
      let paths = jim hs
      lazy (paths |> Seq.forall valid)
      
  ]