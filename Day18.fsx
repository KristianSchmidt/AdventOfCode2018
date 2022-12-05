#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 18
    |> Array.mapi (fun y s -> s.ToCharArray() |> Array.mapi (fun x c -> ((x,y),c)))
    |> Array.collect id
    |> Map.ofArray

let score s =
    (Map.filter (fun k v -> v = '#') s |> Map.count)*
    (Map.filter (fun k v -> v = '|') s |> Map.count)

let nextState (state : Map<int*int,char>) =
    let getNeighbors t (x,y) =
        [| (x-1,y-1); (x,y-1); (x+1,y-1)
           (x-1,y);            (x+1,y)
           (x-1,y+1); (x,y+1); (x+1,y+1)
        |]
        |> Array.choose (fun c -> Map.tryFind c state)
        |> Array.filter ((=)t)
        |> Array.length
    
    let processField (x,y) c =
        match c with
        | '.' when (getNeighbors '|' (x,y)) >= 3 -> '|'
        | '|' when (getNeighbors '#' (x,y)) >= 3 -> '#'
        | '#' when (getNeighbors '#' (x,y)) >= 1 &&
                   (getNeighbors '|' (x,y)) >= 1 -> '#'
        | '#' when (getNeighbors '#' (x,y)) = 0 ||
                   (getNeighbors '|' (x,y)) = 0 -> '.'
        | _ -> c

    state |> Map.map processField
    
let processed =
    [1..10]
    |> List.fold (fun s _ -> nextState s) data

let ans1 = score processed

ans1

// Part 2

let toString (state : Map<int*int,char>) =
    state
    |> Map.toArray
    |> Array.sortBy (fun ((x,y),_) -> y*1000 + x)
    |> Array.map snd
    |> (fun arr -> String(arr))

let process3 initState =
    let rec f i state cache =
        let next = nextState state
        let nIdx = i + 1
        //printfn "%i: %i" (i+1) (score next)
        match Map.tryFind (toString next) cache with
        | Some j ->
            //printfn "%i matches %i" nIdx j
            (next, nIdx,nIdx-j)
        | None ->
            let newCache = cache |> Map.add (toString next) nIdx
            f nIdx next newCache

    let (nState, init, gap) = f 0 initState Map.empty

    //printfn "Init: %i - Gap: %i" init gap
    
    let rem = (1_000_000_000 - init) % gap

    let sol = [1..rem] |> List.fold  (fun s _ -> nextState s) nState

    sol

let res = process3 data

let ans2= score res

ans2
