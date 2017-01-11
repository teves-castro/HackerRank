module Tests

open Expecto
open FsCheck
open FsCheck.Arb
open ExpectoFsCheck
open Hacker
open Problems
open Recursion
open DynamicProgramming

let config = { FsCheck.Config.Default with MaxTest = 10 }

[<Tests>]
let tests =
  testList "Hacker ranks test" [

    testCase "Maximum Sub Array" <| fun _ ->
      let subject = [| "3"; "4"; "1 2 3 4"; "6"; "2 -3 2 3 4 -5"; "4"; "-1 2 -3 -4"|] |> hacker maximumSubArray
      Expect.equal subject [|"10 10"; "9 11"; "2 2"; ""|] "Failed"

    testProperty "Add" <| fun a b ->
      a + b = b + a

    testProperty "Fibonacci sequence add subsequent elements" <| fun a ->
      let nthfib n = fibs |> Seq.skip (int n) |> Seq.head
      let f1 = nthfib a
      let f2 = nthfib (a + 1u)
      let f3 = nthfib (a + 2u)
      f1 + f2 = f3
  ]