module Tests

open Expecto
open Hacker
open Problems
open Recursion
open DynamicProgramming

[<Tests>]
let tests =
  testList "Hacker ranks test" [

    testCase "Maximum Sub Array" <| fun _ ->
      let subject = [| "3"; "4"; "1 2 3 4"; "6"; "2 -3 2 3 4 -5"; "4"; "-1 2 -3 -4"|] |> hacker maximumSubArray
      Expect.containsAll subject [|"10 10"; "9 11"; "2 2"; ""|] "Failed"
  ]