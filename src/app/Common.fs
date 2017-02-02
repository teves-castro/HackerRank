module Common

open System

let split (s: String) = s.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries) 

let allPairs source1 source2 =
    let cached = Seq.cache source2
    source1 |> Seq.collect (fun x -> cached |> Seq.map (fun y -> x,y))
