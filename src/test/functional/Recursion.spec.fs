module Recursion.Tests

open Expecto
open FsCheck
open FsCheck.Arb
open ExpectoFsCheck
open Hacker
open Recursion

let config = { FsCheck.Config.Default with MaxTest = 10 }

[<Tests>]
let tests =
  testList "Sierpinski" [
    testProperty "line count should be 32" <| fun i ->
      i >= 0 ==>
      let triangle = sierpinski i
      1 = 2

    testProperty "one count" <| fun i ->
      i >= 0 ==>
      let oneCount = sierpinski i |> Seq.filter (fun c -> c = '1') |> Seq.length
      oneCount = 63 * 32 / 2 + 1
  ]