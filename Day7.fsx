#load "Helpers.fsx"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let parents =
    Helpers.Web.getInput 7
    |> Array.map (fun s -> s.Replace("Step ", "").Replace(" can begin.", "").Replace(" must be finished before step ", ",").Split(','))
    |> Array.map (fun [|a1;a2|] -> a2,a1)
    |> Array.groupBy fst
    |> Array.map (fun (e1,e2) -> e1, Array.map snd e2 |> Set.ofArray)


let allVertices =
    let v1 = parents |> Array.collect (snd >> Set.toArray)
    let v2 = parents |> Array.map fst
    Array.concat [|v1;v2|]
    |> Array.distinct
    |> Set.ofArray

let parentsMap = Map.ofArray parents

let orphans = Set.difference allVertices (Map.keys parentsMap |> Set.ofSeq)

let orphansMap =
    Set.toArray orphans
    |> Array.map (fun x -> x, Set.empty<string>)
    |> Map.ofArray

let startingMap = Helpers.Map.merge parentsMap orphansMap


let rec f (map : Map<string,Set<string>>) xs =
    let avail =
        Map.filter (fun k v -> v = Set.empty) map
        |> Map.keys |> Seq.toArray
        |> Array.sort
        |> Array.tryHead

    match avail with
    | Some s ->
        printfn "%s" s
        let map' = map |> Map.map (fun k v -> Set.remove s v) |> Map.remove s
        f map' (s :: xs)
    | None -> List.rev xs |> (fun x -> String.Join("", x))

f startingMap []

// Part 2

let time (s : string) = s |> char |> int |> (fun i -> i - 65 + 1 + 60)

module Array =
    let tryTake i arr =
        let l = Array.length arr
        Array.take (min i l) arr

let solve workers map =
    let rec f currTime currWork map doneOrder =
        let avail =
            Map.filter (fun k v -> v = Set.empty) map
            |> Map.keys |> Seq.toArray
            |> Array.sort
            |> Array.filter (fun k -> Array.exists (fst >> (=)k) currWork |> not)
            |> Array.tryTake (workers - Array.length currWork)

        let chosen = avail |> Array.map (fun a -> a, currTime + time a)

        printfn "Time: %i. Chosen: %A" currTime chosen

        let newWork = Array.append currWork chosen

        match newWork.Length with
        | 0 -> currTime + 1
        | _ ->
            let (finItem, finTime) = newWork |> Array.minBy snd
            
            let newMap =
                map |> Map.map (fun k v -> Set.remove finItem v) |> Map.remove finItem
            let newOrder = finItem :: doneOrder
            let finalNewWork = Array.filter ((<>)(finItem,finTime)) newWork
            printfn "Done: %s" finItem
            printfn "%A" (finalNewWork |> Array.sortBy snd)
            printfn "---"
            f finTime finalNewWork newMap newOrder

    f -1 [||] map []

solve 5 startingMap


