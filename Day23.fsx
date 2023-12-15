#load "Helpers.fsx"

#time "on"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let parse s =
    match s with
    | Regex "pos=<(\-?\d+),(\-?\d+),(\-?\d+)>, r=(\d+)" [x;y;z;r] ->
        ((int64 x,int64 y,int64 z),int64 r)

let data =
    Helpers.Web.getInput 23
    |> Array.map parse

let largest = data |> Array.maxBy snd

let manhattan (x1,y1,z1) (x2,y2,z2) =
    abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

let manToLargest c = manhattan (fst largest) c

let ans1 =
    data
    |> Array.map fst
    |> Array.filter (fun c -> (manToLargest c) <= (snd largest))
    |> Array.length

// Part 2

(*
    General procedure
    1. Determine which clusters are overlapping
    2. Make a graph with cluster idx as vertices and an edge if clusters are overlapping
    3. Use Bron-Kerbosh algorithm to find the largest clique in the graph
    4. Assume that if there is a clique where all clusters have overlap with each other, then
       there must be at least one point where they all overlap simultaneously
    5. Compute a naive centroid of all the clusters in the clique
    6. Make a scoring function that is the sum of "too far away" distances for all clique clusters
       for which the given point is not overlapping
    7. Make a wide search that gradient descends quickly towards a better point based on the above score
    8. When close enough, switch to a final search where each axis is modified individually
    9. The search finds the only (in my input case) point that is reachable by all clusters,
       compute the manhattan distance to 0 and we're done
*)

let areOverlapping i j =
    let dist = manhattan (fst data[i]) (fst data[j])
    let radiusSum = (snd data[i]) + (snd data[j])
    dist <= radiusSum

let edges =
    seq {
        for i in 0 .. (data.Length-2) do
            for j in (i+1)..(data.Length-1) do
                if (areOverlapping i j) then
                    yield (i,j)
                    yield (j,i)
    }

let neighbors =
    edges
    |> Seq.groupBy fst
    |> Seq.map (fun (i, s) -> i, s |> Seq.map snd |> Set.ofSeq)
    |> Map.ofSeq

// Impl based on pseudocode from https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
let rec bronKerbosch1(r : Set<int>, p : Set<int>, x : Set<int>) =
    if (p.IsEmpty && x.IsEmpty) then
        //printfn "Length: %i - Set: %A" r.Count r
        Some r
    else
        let rec loop (p',x',vs) =
            match vs with
            | v :: vs' ->
                let resOpt = bronKerbosch1(Set.add v r, Set.intersect p' neighbors[v], Set.intersect x' neighbors[v])
                match resOpt with
                | Some r -> Some r
                | None -> loop (Set.remove v p', Set.add v x', vs')
            | [] -> None

        loop (p,x,Set.toList p)

let clique = bronKerbosch1 (Set.empty, Set.ofList [0..data.Length-1], Set.empty)

let inClique =
    clique
    |> Option.get
    |> Set.toArray

let n = inClique |> Array.length |> int64

// Verifies that all pairs in the clique are indeed neighbors
for i in inClique do
    for j in inClique do
        if (i <> j && (not (areOverlapping i j))) then
            printfn "%i - %i => %A" i j (areOverlapping i j)

let cliqueCoords =
    inClique
    |> Array.map (fun i -> fst data[i])
    
let centroid =
    cliqueCoords
    |> Array.reduce (fun (x,y,z) (x',y',z') -> (x+x', y+y', z+z'))
    |> (fun (x,y,z) -> (x/n, y/n, z/n))

let score c =
    inClique
    |> Array.filter (fun i -> manhattan (fst data[i]) c <= (snd data[i]) |> not)
    |> Array.sumBy (fun i -> (snd data[i]) - (manhattan (fst data[i]) c))

let direction cFrom cTo =
    let (x : int64, y : int64, z : int64) = cFrom
    let (x',y',z') = cTo
    int64 (Math.Sign(x'-x)), int64 (Math.Sign(y'-y)), int64 (Math.Sign(z'-z))

let sum3d (x,y,z) (i,j,k) = (x+i,y+j,z+k)

let rec searchWide p =
    let score1 = score p
    if (score1 > -100) then
        //printfn "Found it! P = %A" p
        p
    else
        //printfn "Score: %i" score1
        let towardsPoint =
            inClique
            |> Array.filter (fun i -> manhattan (fst data[i]) p <= (snd data[i]) |> not)
            |> Array.minBy (fun i -> (snd data[i]) - (manhattan (fst data[i]) p))
            |> fun i -> fst data[i]

        let (dX,dY,dZ) = direction p towardsPoint
        let rate =
            if (score1 > -1_000_000L) then
                10L
            else if (score1 > -1_000L) then
                1L
            else
                10_000L
        searchWide (sum3d p (rate*dX,rate*dY,rate*dZ))

let searchedPoint = searchWide centroid

let finalSearch start =
    let mutable cache : Set<int64*int64*int64> = Set.empty
    let rec searchPerDirection (x,y,z) currScore =
        //printfn "%A - %i" (x,y,z) currScore
        cache <- Set.add (x,y,z) cache
        if (currScore >= 0L) then
            [|(x,y,z)|]
        else
            let candidates =
                [| (x+1L,y,z)
                   (x-1L,y,z)
                   (x,y+1L,z)
                   (x,y-1L,z)
                   (x,y,z+1L)
                   (x,y,z-1L)
                   
                   (x+1L,y+1L,z)
                   (x+1L,y-1L,z)
                   (x-1L,y+1L,z)
                   (x-1L,y-1L,z)
                   
                   (x+1L,y,z+1L)
                   (x+1L,y,z-1L)
                   (x-1L,y,z+1L)
                   (x-1L,y,z-1L)
    
                   (x,y+1L,z+1L)
                   (x,y+1L,z-1L)
                   (x,y-1L,z+1L)
                   (x,y-1L,z-1L)
    
                   |]
                |> Array.map (fun c -> c, score c)
                |> Array.filter (fun (c,s) -> s >= currScore && not (Set.contains c cache))
            
            let res = candidates |> Array.collect (fun (c,s) -> searchPerDirection c s)
            if (res.Length <> 0) then
                //printfn "Found %i solutions" res.Length
                //res |> Array.iter (fun c -> printfn "%A" c)
                res
            else
                [||]

    searchPerDirection start (score start)

let sol = finalSearch searchedPoint |> Array.head

let ans2 = manhattan sol (0,0,0)
