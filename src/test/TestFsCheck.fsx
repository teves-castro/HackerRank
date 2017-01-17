// sets the current directory to be same as the script directory
System.IO.Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__)

// assumes nuget install FsCheck.Nunit has been run 
// so that assemblies are available under the current directory
#I @"../../packages/FsCheck/lib/net45"
#I @"../../packages/NUnit/lib"

#r @"FsCheck.dll"
#r @"nunit.framework.dll"

open System
open FsCheck
open NUnit.Framework

let ordered xs = xs |> Seq.pairwise |> Seq.forall (fun (a,b) -> b >= a )

let rec insert a xs = 
    match xs with
    | [] -> [a]
    | x::rest -> if a > x then x::(insert a rest) else a::xs

let insertTrivial (x:int) xs = 
  ordered xs ==> (ordered (insert x xs))
  |> Prop.trivial (List.isEmpty xs)
Check.Quick insertTrivial

let orderedList = Arb.from<list<int>> |> Arb.mapFilter List.sort ordered

let insertWithArb x = Prop.forAll orderedList (fun xs -> ordered(insert x xs))
Check.Quick insertWithArb

let insertClassify (x:int) xs = 
  ordered xs ==> (ordered (insert x xs))
  |> Prop.classify (ordered (x::xs)) "at-head"
  |> Prop.classify (ordered (xs @ [x])) "at-tail"
Check.Quick insertClassify
