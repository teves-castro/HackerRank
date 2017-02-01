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

module List =

    // The apply function for lists
    // [f;g] apply [x;y] becomes [f x; f y; g x; g y]
    let apply (fList: ('a->'b) list) (xList: 'a list)  = 
        [ for f in fList do
          for x in xList do
              yield f x ]
