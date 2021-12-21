#load "Helpers.fsx"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 10
    |> Array.map (fun s -> s.Replace("position=<","").Replace("> velocity=<", ",").Replace(">",""))
    |> Array.map (Helpers.split "," >> Array.map (fun s -> s.Trim()))
    |> Array.map (fun [|d1;d2;d3;d4|] -> (int d1, int d2),(int d3, int d4))

let render data' =
    let rec f data lastDist =        
        let newData =
            data
            |> Array.map (fun ((x,y),(vx,vy)) -> (x+vx,y+vy),(vx,vy))

        let (xmin,xmax,ymin,ymax) = Helpers.getMinMax newData
        let dist = xmax-xmin+ymax-ymin
        
        if (dist > lastDist) then
            data
        else
            f newData dist
    
    f data' Int32.MaxValue

let test = render data
    
let keys =
    test
    |> Array.map fst
    |> Set.ofArray

let (xmin,xmax,ymin,ymax) = Helpers.getMinMax test

for y in ymin .. ymax do
    printfn ""
    for x in xmin .. xmax do
        let c = if (Set.contains (x,y) keys) then "#" else "."
        printf "%s" c

