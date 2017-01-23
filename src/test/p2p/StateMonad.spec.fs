module StateMonad.Spec

open Expecto
open FsCheck
open FsCheck.Arb
open StateMonad

let config = { FsCheck.Config.Default with MaxTest = 100000 }

[<Tests>]
let tests =
    testList "State" [
        
        testPropertyWithConfig config "what you put is what you get" <| fun a b c ->
            state { 
                do! putS (a+b)
                let! r = getS
                return r
            } |> runS c = (a+b,a+b)

        testPropertyWithConfig config "only the last put count" <| fun a b c d ->
            state { 
                do! putS d
                do! putS (a+b)
                let! r = getS
                return r
            } |> runS c = (a+b,a+b)

        ftestList "monad Laws" [
            testProperty "Left Identity" <| fun (x:int) f (s:int) ->
                let left = (state { let! x' = state { return x } in return! f x' } |> runS s) 
                let right = (state { return! f x } |> runS s)
                if left <> right then printfn "%A = ? = %A" left right else printf ""
                left = right

            testProperty "Right Identity" <| fun x comp s ->
                (state { let! x = comp in return x } |> runS s) = (state { return! comp } |> runS s)

            testProperty "Associativity" <| fun comp f g s ->
                let left = 
                    state { 
                        let! x = comp 
                        let! y = f x
                        return! g y 
                    } |> runS s
                let right = 
                    state { 
                        let! y = state { 
                            let! x = comp
                            return! f x
                        }
                        return! g y
                    } |> runS s
                left = right
        ]
    ]
