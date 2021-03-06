module Addition.Spec

open Expecto
open FsCheck
open FsCheck.Arb
open Hacker
open Recursion

let config = { FsCheck.Config.Default with EndSize = 10; MaxTest = 2000; }

module Addition = 
    let addv1 x y = if x=1 && y=2 then 3 else 0    
    let addv2 x y = if x=1 && y=2 then 3 else if x=2 && y=2 then  4 else 0
    let addv3 x y = x * y
    let addv4 x y = 0
    let addv5 x y = y - x
    let addv6 x y = 
        if (x < 10) || (y < 10) then
            x + y  // correct for low values
        else
            x * y 
    let addv7 x y = 
        if (x < 225) || (y < 225) then
            x + y  // correct for low values
        else
            x * y  // incorrect for high values
    let correctAdd a b = a + b

let add = Addition.correctAdd

[<Tests>]
let tests =
    testList "Addition" [
        
        testProperty "must be commutative" <| fun a b ->
            add a b = add b a

        testProperty "must be associative" <| fun a b c ->
            add (add a b) c = add a (add b c)

        testProperty "must have neutral element" <| fun a ->
            add a 0 = a

    ]

type Tree = Leaf of int | Branch of Tree * Tree

let tree =
    let rec tree' s = 
        match s with
        | 0 -> Gen.map Leaf Arb.generate<int>
        | n when n>0 -> 
            let subtree = tree' (n/2)
            Gen.oneof [ Gen.map Leaf Arb.generate<int> 
                        Gen.map2 (fun x y -> Branch (x,y)) subtree subtree]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized tree'

type MyGenerators =
  static member Tree() =
      {new Arbitrary<Tree>() with
          override x.Generator = tree
          override x.Shrinker t = Seq.empty }

Arb.register<MyGenerators>() |> ignore
