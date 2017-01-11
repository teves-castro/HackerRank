module Problems

open System
open Hacker

let clinics () =
    let data = readIntArray ()
    let totalCities = data.[0]
    let totalClinics = data.[1]

    let target = fun (p,c) -> p/c
    let neg = (*) -1

    let populationByCity =
        [0..totalCities-1]
        |> List.map (fun _ -> readInt (), 1)
        |> List.sortBy (target >> neg)

    let spareClinics = totalClinics - totalCities

    let rec distribute distribution spare =
        if spare = 0 then
            distribution
        else
            let sorted = distribution |> List.sortBy (target >> neg)
            let population, clinics = sorted |> List.head
            let adjusted = (population, clinics+1) :: (sorted |> List.tail)
            distribute adjusted (spare-1)

    distribute populationByCity spareClinics
    |> Seq.map target
    |> Seq.head
    |> sprintf "%d"
    |> writeLine


let clinics' () =
    let data = readIntArray ()
    let totalCities = data.[0]
    let totalClinics = data.[1]

    let populationByCity' =
        [0..totalCities-1]
        |> List.map (fun _ -> readInt ())

    let totalPopulation = populationByCity' |> List.sum

    let populationByCity =
        populationByCity'
        |> List.map (
            (fun p -> p, totalClinics * p / totalPopulation) >> 
            (fun (p, c) -> if c>0 then p,c else p,1)
        )                 // At least one clinic by city, this might over allocate clinics

    let spareClinics = totalClinics - (populationByCity |> List.map snd |> List.sum)

    let rec distribute distribution (spare:int) =
        if spare = 0 then
            distribution
        else
            let adjustment = spare / spare
            let sorted = distribution |> List.sortBy (fun (p,c) -> p/c * adjustment)
            let population, clinics = sorted |> List.head
            let adjusted = (population, clinics+adjustment) :: (sorted |> List.tail)
            distribute adjusted (spare-adjustment)

    distribute populationByCity spareClinics
    |> Seq.map (fun (p,c) -> p/c)
    |> Seq.head
    |> sprintf "%d %d" spareClinics
    |> writeLine

let clinics'' () =
    let data = readIntArray ()
    let totalCities = data.[0]
    let totalClinics = data.[1]

    let populationByCity =
        [0..totalCities-1]
        |> List.map (fun _ -> readInt ())

    let spareClinics = totalClinics - totalCities

    let totalPopulation =
        populationByCity
        |> List.sum

    let clinicsByCity =
        populationByCity
        |> List.map (
            (fun p -> totalClinics * p / totalPopulation) >>
            (fun c -> if c>0 then c else 1)
        )             // At least one clinic by city, this might over allocate clinics

    let spareClinics = totalClinics - (clinicsByCity |> List.sum)

    let maxRequiredKits clinicsByCity =
        populationByCity
        |> List.zip clinicsByCity
        |> List.map (fun (c, p) -> p / c)
        |> List.max

    let rec adjust clinicsByCity spare level =
        if spare = 0 || level > 10 then
            clinicsByCity
        else
            let totalAdjustableClinics =
                clinicsByCity
                |> List.filter (fun c -> c > 1)
                |> List.sum

            let adjustments =
                clinicsByCity
                |> List.map (fun c -> if c > 1 then spareClinics * c / totalAdjustableClinics else 0)

            let adjusted =
                adjustments
                |> List.zip clinicsByCity
                |> List.map (fun (c,a) -> c + a)

            adjust adjusted (totalClinics - (adjusted |> List.sum)) (level+1)

    let adjusted = adjust clinicsByCity spareClinics 0

    sprintf "%A" (maxRequiredKits clinicsByCity) |> writeLine

// [|
//     "6 7";
//     "200000";
//     "500000";
//     "900000";
//     "10000";
//     "5000";
//     "1";
//     "200";
//     "700";
// |] |> hacker clinics'

// Euler #1: Multiples of 3 and 5
let euler1 () =
    let solve n =
        let n = n - 1UL
        let n3 = n / 3UL
        let n5 = n / 5UL
        let n15 = n / 15UL
        let s3 = 3UL * ((pown n3 2) + n3) / 2UL
        let s5 = 5UL * ((pown n5 2) + n5) / 2UL
        let s15 = 15UL * ((pown n15 2) + n15) / 2UL
        s3 + s5 - s15
    
    [1 .. readInt()]
    |> Seq.map (readInt >> uint64 >> solve ) 
    |> Seq.iter (sprintf "%d" >> writeLine)

// Euler #2: Even Fibonacci numbers
let euler2 () =
    let evenFibs = seq {
        yield 2UL;
        let rec loop f1 f2 = seq {
            let next = f1 + f2
            if next % 2UL = 0UL then yield next
            yield! loop f2 next 
        }
        yield! loop 1UL 2UL
    }

    [1 .. readInt()]
    |> Seq.map (
        (readUInt64)
        >> (fun n -> evenFibs |> Seq.takeWhile (fun f -> f <= n))
        >> (Seq.fold (+) 0UL)
    )
    |> Seq.iter (sprintf "%d" >> writeLine)

// Pascal
let pascal =
    let cl x xs = seq { 
        yield x
        yield! xs
    }
    let cr xs x = seq { 
        yield! xs
        yield x
    }

    let rec pascal' previous = seq {
        yield previous
        let next = (0UL |>cl<| previous) |> Seq.zip <| (previous |>cr<| 0UL) |> Seq.map (fun (x,y) -> x+y)
        yield! pascal' next
    }
    pascal' [1UL]

let pascal1 n = 
    let rec nextrow row y ys =
        match row with
        | [] -> 1UL::ys
        | x::xs -> nextrow xs x ((x + y)::ys)
    and addrow i row rows = 
        if i <= 1UL then row::rows
        else addrow (i-1UL) (nextrow row 0UL []) (row::rows)
    addrow n [1UL] [] 

let fibs =
    let rec fibs' f1 f2 = seq { 
        yield f1
        yield! fibs' f2 (f1+f2) 
    }
    fibs' 1I 1I
