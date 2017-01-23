module MaybeMonad.Spec

open Expecto
open FsCheck
open FsCheck.Arb
open MaybeMonad

let config = { FsCheck.Config.Default with MaxTest = 10000 }

[<Tests>]
let tests =
    testList "Maybe" [
        ptestList "monad Laws" [
            testPropertyWithConfig config "Maybe left Identity" <| fun x f s ->
                let left = (maybe { let! x' = maybe { return x } in return! f x' } |> runM)
                let right = (maybe { return! f x } |> runM)
                if left <> right then printfn "%A = ? = %A" left right else printf ""
                left = right

            testProperty "Maybe Right Identity" <| fun x comp s ->
                (maybe { let! x = comp in return x } |> runM) = (maybe { return! comp } |> runM)

            testPropertyWithConfig config "Maybe Associativity" <| fun comp f g s ->
                let left = 
                    maybe { 
                        let! x = comp 
                        let! y = f x
                        return! g y 
                    } |> runM
                let right = 
                    maybe { 
                        let! y = maybe { 
                            let! x = comp
                            return! f x
                        }
                        return! g y
                    } |> runM
                if left <> right then printfn "%A = ? = %A" left right else printf ""
                left = right
        ]
    ]
