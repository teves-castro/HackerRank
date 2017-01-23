module StateMonad.Spec

open Expecto
open FsCheck
open FsCheck.Arb
open StateMonad

let config = { FsCheck.Config.Default with MaxTest = 1000 }

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

        testList "monad Laws" [
            testProperty "Left Identity" <| fun x f s ->
                ((f x |> runS s) = (f x |> runS s)) ==>
                (runS s (state { let! x' = state { return x } in return! f x' }) = runS s (state { return! f x }))

            testPropertyWithConfig config "Left identity" <| fun m is ->
                runS is (state.Bind (m, state.Return)) = runS is m

            testPropertyWithConfig config "Right identity" <| fun f x is ->
                runS is (state.Bind (state.Return x, f)) = runS is (f x)

            testPropertyWithConfig config "Associativity" <| fun f g m is x ->
                (runS is (f x) = runS is (f x) && runS is (g x) = runS is (g x)) ==>
                (runS is (state.Bind (state.Bind (m, f), g)) = runS is (state.Bind (m, (fun x -> state.Bind (f x, g)))))
        ]
    ]
