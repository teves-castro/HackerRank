module Hacker

open System
open System.IO
open System.Text

let mutable readLine = Console.ReadLine
let mutable writeLine  = fun (o : obj) -> printfn "%A" o
let mutable write  = fun (o : obj) -> printf "%A" o

let hacker problem input =
    let text = input |> Seq.fold (fun s l -> s + l + "\n") "" 

    let reader = new StringReader(text)

    readLine <- reader.ReadLine

    let sb = new StringBuilder ()
    let writer = new StringWriter(sb)

    writeLine <- writer.WriteLine
    write <- writer.Write

    // Console.SetOut writer

    problem ()

    sb.ToString().Split('\n')


let readInt _ = readLine () |> int
let readUInt64 _ = readLine () |> uint64
let readBigint _ = readLine () |> bigint.Parse

let readIntArray() =
    let line = readLine ()
    line.Split(' ') |> Array.map int

// let readIntSquareMatrix() =
//     let size = readInt()
//     size,
//     [| for i in 0..(size - 1) -> readIntArray() |]

let readIntMatrix lines =
    [| for i in 0..(lines - 1) -> readIntArray() |]


// Utils
let split ls = ls |> Seq.map fst, ls |> Seq.map snd
let tupleApply g t = t |> (fun (f, s) -> (f |> g, s |> g))
let (||>) t g = tupleApply g t

let tryHead ns =
    match ns with
    | _ when Seq.isEmpty ns -> None
    | _ -> Some (ns |> Seq.head)

let toBinary (n:bigint)=
    let bytes = n.ToByteArray()
    let idx = bytes.Length - 1

    // Create a StringBuilder having appropriate capacity.
    let base2 = new StringBuilder(bytes.Length * 8)

    // Convert first byte to binary.
    let binary = Convert.ToString(bytes.[idx], 2)

    // Ensure leading zero exists if value is positive.
    if binary.[0] <> '0' && n.Sign = 1 then
        base2.Append('0') |> ignore

    // Append binary string to StringBuilder.
    base2.Append(binary) |> ignore

    // Convert remaining bytes adding leading zeros.
    for idx in (idx-1) .. -1 .. 0 do //   (idx--; idx >= 0; idx--)
        base2.Append(Convert.ToString(bytes.[idx], 2).PadLeft(8, '0')) |> ignore

    let str = base2.ToString()
    str.Substring(str.IndexOf('1'))

