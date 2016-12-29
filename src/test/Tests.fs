module Hacker

open NUnit.Framework
open FsUnit
open Hacker
open Problems
open Recursion
open DynamicProgramming

[<Test>]
let ``Example Test`` () =
    1 |> should equal 1

[<Test>]
let ``Maximum Sub Array`` () =
    [|
        "3";
        "4";
        "1 2 3 4";
        "6";
        "2 -3 2 3 4 -5";
        "4"
        "-1 2 -3 -4";
    |]
    |> hacker maximumSubArray
    |> should equal [|"10 10"; "9 11"; "2 2"; ""|]

// [<Test>]
// let ``Fibonacci Modified`` () =
//     [|"2 2 20"|]
//     |> hacker fibonacciModified
//     |> should equal [|"10 10"; "9 11"; "2 2"; ""|]
