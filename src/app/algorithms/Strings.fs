module Strings

open System
open Hacker


// Super Reduced String
let superReducedString () = 
    let (|Empty|Cons|) (xs: 'a seq) =
        if Seq.isEmpty xs then Empty
        else Cons(Seq.head xs, Seq.skip 1 xs)

    let rec removeDoubles s =
        seq {
            match s with
            | [] -> ()
            | [h] -> yield h
            | h1::h2::t when h1 = h2 -> 
                yield! removeDoubles t
            | h1::h2::t when h1 <> h2 ->
                yield h1
                yield! removeDoubles (h2 :: t)
            | _ -> failwith "impossible"
        } |> Seq.toList
    
    and loop s =         
        match removeDoubles s with
        | [] -> []
        | r when s = r -> r |> Seq.toList
        | r -> loop r
    
    match readLine () |> Seq.toList |> loop with
    | [] -> printfn "Empty String"
    | l -> l |> Seq.map string |> String.concat "" |> printfn "%s"


// Camel Case
let camelCase () = 
    (readLine() |> Seq.filter (fun c -> int c < 97) |> Seq.length) + 1
    

// Morgan and a String
let morganAndAString () =
    let solve sa sb =
        let alength = sa |> Array.length // 4
        let blength = sb |> Array.length // 3

        let smaller ai bi =
            let i, j = ref ai, ref bi // 1,1 |
            while !i < alength && !j < blength && sa.[!i] = sb.[!j] do
                i := !i + 1
                j := !j + 1

            match !i, !j with
            | _ when !j = blength -> true
            | _ when !i = alength -> false
            | _ -> sa.[!i] <= sb.[!j]

        let recursion = ref 0

        let rec solve' ai bi acum =
            recursion := !recursion + 1

            match ai, bi with // 0,0 | 1,0 | 1,1 |
            | _ when ai = alength ->
                acum |> List.append (sb |> Seq.skip bi |> List.ofSeq |> List.rev)

            | _ when bi = blength ->
                acum |> List.append (sa |> Seq.skip ai |> List.ofSeq |> List.rev)

            | _, _ ->
                let a, b = sa.[ai], sb.[bi] // J,L | Z,L | Z,Z
                match a, b with
                | _ when a < b -> solve' (ai+1) bi (a::acum)
                | _ when b < a -> solve' ai (bi+1) (b::acum)
                | _ ->
                    if (smaller ai bi) then
                        solve' (ai+1) bi (a::acum)
                    else
                        solve' ai (bi+1) (b::acum)

        let result = solve' 0 0 []
        printfn "%d" !recursion
        result

    let printCol s = System.String(s |> Seq.toArray) |> Console.WriteLine

    let n = readInt()
    for i in 0..n - 1 do
        let sa = readLine() |> Array.ofSeq
        let sb = readLine() |> Array.ofSeq
        printCol (solve sa sb |> List.rev)

// [|
//    "1";
//    "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABBBBBBBBBBBBBBBBCCCCDDE";
//    "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABBBBBBBBBBBBBBBBCCCCDDE";
//    "ABACABA";
//    "ABACABA";
//    "JACK";
//    "DANIEL"
// |] |> hacker morganAndAString
