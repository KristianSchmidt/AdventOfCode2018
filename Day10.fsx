#load "Helpers.fsx"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 10
    |> Array.map (fun s -> s.Replace("position=<","").Replace("> velocity=<", ",").Replace(">",""))
    |> Array.map (Helpers.split "," >> Array.map (fun s -> s.Trim()))
    |> Array.map (fun [|d1;d2;d3;d4|] -> (int d1, int d2),(int d3, int d4))

let render data' until =
    let rec f data i =
        if (i = until) then
            data
        else
            let newData =
                data
                |> Array.map (fun ((x,y),(vx,vy)) -> (x+vx,y+vy),(vx,vy))

            if (i % 200 = 0 || i > 10600) then
                let togetherNess =
                    let k = newData |> Array.map fst
                    Array.allPairs k k
                    |> Array.map (fun ((x,y),(x',y')) -> abs (x-x') + abs (y-y'))
                    |> Array.max

                printfn "%i %i" i togetherNess

            //printfn "%i %A" i (Helpers.getMinMax newData)

            f newData (i+1)
    
    f data' 0

let test = render data 10630
    
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

