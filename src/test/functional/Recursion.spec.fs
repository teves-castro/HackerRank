module Recursion.Tests

open System
open Expecto
open FsCheck
open FsCheck.Arb
open ExpectoFsCheck
open Hacker
open Recursion

let config = { FsCheck.Config.Default with MaxTest = 10 }

let split (s: String) = s.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries) 

[<Tests>]
let tests =
  testList "Sierpinski" [
    testProperty "line count should be 32" <| fun i ->
      i >= 0 ==>
      let lines = sierpinski i |> String.Concat |> split |> Seq.length
      printfn "Lines ==> %d" lines
      lines = 32

    // testProperty "one count" <| fun i ->
    //   i >= 0 ==>
    //   let oneCount = sierpinski i |> Seq.filter (fun c -> c = '1') |> Seq.length
    //   printfn "OneCount ==> %d" oneCount
    //   oneCount = 63 * 32 / 2 + 1
  ]