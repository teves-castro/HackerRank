module Tests

open Expecto
open Hacker
open Problems
open Recursion
open DynamicProgramming

[<Tests>]
let tests =
  testList "samples" [
    testCase "universe exists" <| fun _ ->
      let subject = true
      Expect.isTrue subject "I compute, therefore I am."

    testCase "Maximum Sub Array" <| fun _ ->
      let subject = [| "3"; "4"; "1 2 3 4"; "6"; "2 -3 2 3 4 -5"; "4"; "-1 2 -3 -4"|] |> hacker maximumSubArray
      Expect.containsAll subject [|"10 10"; "9 11"; "2 2"; ""|] "Failed"

    testCase "should fail" <| fun _ ->
      let subject = false
      Expect.isTrue subject "I should fail because the subject is false."

  ]