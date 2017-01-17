module Recursion.Tests

open System
open Expecto
open Expecto.Impl
open FsCheck
open FsCheck.Arb
open ExpectoFsCheck
open Hacker
open Recursion

let split (s: String) = s.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries) 

let sierpinskiProps =
  testList "Sierpinski" [
    testProperty "line count should be 32" <| fun i ->
      i >= 0 ==>
      let lines = sierpinski i |> String.Concat |> split |> Seq.length
      lines = 32

    // testProperty "one count" <| fun i ->
    //   i >= 0 ==>
    //   let oneCount = sierpinski i |> Seq.filter (fun c -> c = '1') |> Seq.length
    //   printfn "OneCount ==> %d" oneCount
    //   oneCount = 63 * 32 / 2 + 1
  ]

[<Tests>]
let gdcProps =
  let config = { FsCheck.Config.Default with EndSize = 10 }

  testList "GDC" [

    testProperty "is" <| fun a b ->
      (b > 0 && a > b) ==> lazy (gdc a b = gdc b (a/b))
  ]