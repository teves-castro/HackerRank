// assumes nuget install FsCheck.Nunit has been run 
// so that assemblies are available under the current directory
#I @"../../../Packages/FsCheck/lib/net45"
//#I @"Packages\FsCheck.0.9.2.0\lib\net40-Client"  // use older version for VS2012
#I @"../../../Packages/NUnit/lib"

#r @"FsCheck.dll"
#r @"nunit.framework.dll"

open System
open FsCheck
open NUnit.Framework

// =================================================================================
// State type and associated functions
// =================================================================================

type S<'State,'Value> = 
    S of ('State -> 'Value * 'State)

// encapsulate the function call that "runs" the state
let runS state (S f)  = f state

// lift a value to the S-world 
let returnS x = 
    let run state = 
        x, state
    S run

// lift a monadic function to the S-world 
let bindS f xS = 
    let run state = 
        let x, newState = runS state xS
        runS newState (f x) 
    S run

// ---------------------------------------------------------------------------------
// Creating a state
// ---------------------------------------------------------------------------------

type StateBuilder()=
    member this.Return(x) = returnS x
    member this.ReturnFrom(xS) = xS
    member this.Bind(xS,f) = bindS f xS

let state = new StateBuilder()

// ---------------------------------------------------------------------------------
// Redefining mapS and the others
// ---------------------------------------------------------------------------------

// mapS can be implemented using state
let mapS f xS = state {
    let! x = xS  // unwrap the S<X>
    return f x   // return S of (f x)
    }

// val mapS : f:('a -> 'b) -> M<'a> -> M<'b>

// map2S can be implemented using state
let map2S f xM yM = state {
    let! x = xM  // unwrap M<X>
    let! y = yM  // unwrap M<Y>
    return f x y // return M of (f x y)
    }
// val map2S : f:('a -> 'b -> 'c) -> M<'a> -> M<'b> -> M<'c>

// applyS can be implemented using state
let applyS fM xM = state {
    let! f = fM  // unwrap M<F>
    let! x = xM  // unwrap M<X>
    return f x   // return M of (f x)
    }
// val applyS : M<('a -> 'b)> -> M<'a> -> M<'b>

// short cuts
let (<*>) = applyS
let (<!>) = mapS

// =================================================================================
// Get/Set state
// =================================================================================

let getS = 
    let run state = 
        // return the current state in the first element of the tuple
        state, state
    S run
// val getS : S<State> 

let putS newState = 
    let run _ = 
        // return nothing in the first element of the tuple
        // return the newState in the second element of the tuple
        (), newState
    S run
// val putS : 'State -> S<unit>

let g = Arb.generate<S<int,int>>
let g = Arb.generate<int->Maybe<string>>

let choose = Gen.oneof [ g; g ]

g 
|> Gen.sample 1000 20 
|> List.map (fun f -> 
                    state { 
                        let! _ = f
                        do! putS 222 
                        return 1 
                    } |> runS 0)

