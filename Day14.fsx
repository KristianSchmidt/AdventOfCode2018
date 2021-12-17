#load "Helpers.fsx"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 14 |> Array.head |> int

let start =
    Map.empty
    |> Map.add 0 3
    |> Map.add 1 7

let iter until start =
    let rec f curr1 curr2 count (map : Map<int,int>) =
        if (count >= until) then
            map
        else
            let score1 = map[curr1]
            let score2 = map[curr2]
            let newRecipes, newLength =
                let tot = score1 + score2
                //printfn "Tot: %i" tot
                if (tot >= 10) then
                    [(count,1); (count+1,tot % 10)], count+2
                else
                    [count,tot], count+1

            //printfn "%A" newRecipes

            let newMap = newRecipes |> List.fold (fun m (k,v) -> Map.add k v m) map
            let newCurr1 = (curr1 + 1 + score1) % newLength
            let newCurr2 = (curr2 + 1 + score2) % newLength
            f newCurr1 newCurr2 newLength newMap

    f 0 1 (Map.count start) start

let ans1 =
    iter (data+10) start
    |> Map.toArray
    |> Array.sortBy fst
    |> Array.skip data
    |> Array.take 10
    |> Array.map (snd >> string)
    |> (fun arr -> String.Join("", arr))

// Part 2

let data2 = (string data).ToCharArray() |> Array.map (string >> int)

let ans2 =
    iter (data*100) start
    |> Map.toArray
    |> Array.sortBy fst
    |> Array.map snd
    |> Array.windowed 6
    |> Array.findIndex ((=)data2)


