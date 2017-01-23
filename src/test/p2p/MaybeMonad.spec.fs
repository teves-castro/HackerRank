module MaybeMonad.Spec

open Expecto
open FsCheck
open FsCheck.Arb
open MaybeMonad

let config = { FsCheck.Config.Default with MaxTest = 1000 }

[<Tests>]
let tests =
    testList "Maybe" [
        testList "monad Laws" [
            testPropertyWithConfig config "Left identity" <| fun m ->
                maybe.Bind (m, maybe.Return) = m

            testPropertyWithConfig config "Right identity" <| fun f x is ->
                (f x = f x) ==> (maybe.Bind (maybe.Return x, f) = f x)

            testPropertyWithConfig config "Associativity" <| fun f g m is x ->
                (f x = f x) ==> (maybe.Bind (maybe.Bind (m, f), g) = maybe.Bind (m, (fun x -> maybe.Bind (f x, g))))
        ]
    ]
