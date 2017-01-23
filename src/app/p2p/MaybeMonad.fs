module MaybeMonad

type Maybe<'a> = Success of 'a | Failure

let returnM a = Success a

let bindM f = function
    | Failure -> Failure
    | Success a -> f a

type MaybeBuilder() =
    member this.Return(x) = returnM x
    member this.ReturnFrom(x) = x
    member this.Bind(xM, f) = bindM f xM

let maybe = MaybeBuilder()

let runM = function
    | Failure -> None
    | Success x -> Some x