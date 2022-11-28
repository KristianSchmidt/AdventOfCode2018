#load "Helpers.fsx"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let initState =
    Helpers.Web.getInput 12
    |> Array.head
    |> fun s -> s.Substring(15).ToCharArray()
    |> Array.mapi (fun i c -> (i,c))
    |> Map.ofArray

let rules =
    Helpers.Web.getInput 12
    |> Array.tail
    |> Array.tail
    |> Array.map (Helpers.split " => ")
    |> Array.map (fun [|k;v|] -> (k,char v))
    |> Map.ofArray

let potAt state i =
    Map.tryFind i state
    |> Option.defaultValue '.'

let rowAt state i =
    let pot i = potAt state i |> string
    (pot (i-2)) + (pot (i-1)) + (pot i) + (pot (i+1)) + (pot (i+2))

let progressState state =
    let existing =
        state
        |> Map.map (fun k v -> Map.find (rowAt state k) rules)

    let lowestNumber =
        state |> Map.keys |> Seq.min

    let highestNumber = 
        state |> Map.keys |> Seq.max

    let newKeys =
        Array.concat [|[|lowestNumber-5 .. lowestNumber|];[|highestNumber .. highestNumber + 5|]|]
        |> Array.map (fun k -> k, Map.find (rowAt state k) rules)

    newKeys |> Array.fold (fun s (k,v) -> Map.add k v s) existing

let ans1 =
    [|1..20|]
    |> Array.fold (fun s _ -> progressState s) initState
    |> Map.toArray
    |> Array.filter (snd >> ((=)'#'))
    |> Array.sumBy fst

// Part 2

let ansGen g =
    [|1..g|]
    |> Array.fold (fun s _ -> progressState s) initState
    |> Map.toArray
    |> Array.filter (snd >> ((=)'#'))
    |> Array.sumBy fst

// At some point the increment between generation stays
// the same, so we can just do X iterations and multiply the rest of the way
let ans2 = int64 (ansGen 2000) + (62L * (50000000000L - 2000L))