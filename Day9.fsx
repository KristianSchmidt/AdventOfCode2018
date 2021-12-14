#load "Helpers.fsx"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let (players, lastMarble) =
    Helpers.Web.getInput 9
    |> Array.collect (fun s -> s.Split("; "))
    |> Array.map (fun s -> s.Replace(" players", "").Replace(" points",""))
    |> Array.map (fun s -> s.Replace("last marble is worth ", ""))
    |> (fun [|x;y|] -> (int x,int y))


let doIter players (lst, currIdx, scores, player) num =
    let nextPlayer = (player+1)%players
    let length = List.length lst
    match num with
    | x when x % 23 = 0 ->
        let currScore = scores |> Map.tryFind player |> Option.defaultValue 0
        let removeIdx = (currIdx - 7 + length) % length
        let newScore = currScore + num + lst[removeIdx]
        let newList = lst |> List.removeAt removeIdx
        (newList, removeIdx, scores |> Map.add player newScore, nextPlayer)
    | _ ->
        let nextIdx = ((currIdx + 1) % length) + 1
        List.insertAt nextIdx num lst, nextIdx, scores, nextPlayer


let (_,_,scores,_) =
    [1..lastMarble]
    |> List.fold (doIter players) ([0], 0, Map.empty, 1)

let ans1 =
    scores
    |> Map.toArray
    |> Array.maxBy snd
    |> snd

// Part 2

open System.Collections.Generic

let mutable lst = LinkedList<int64>([0L])

let prev (node : LinkedListNode<int64>) =
    match node.Previous with
    | null -> lst.Last
    | x -> x

let doIterLL players (length, currNode : LinkedListNode<int64>, scores, player) num =
    let nextPlayer = (player+1)%players
    match num with
    | x when x % 23L = 0L ->
        let currScore = scores |> Map.tryFind player |> Option.defaultValue 0L
        let removeNode = currNode |> prev |> prev |> prev |> prev |> prev |> prev |> prev
        let newScore = currScore + num + removeNode.Value
        let next = removeNode.Next
        lst.Remove(removeNode)
        (length - 1, next, scores |> Map.add player newScore, nextPlayer)
    | _ ->
        let nextNode =
            match currNode.Next with
            | null -> lst.First
            | x -> x
        lst.AddAfter(nextNode, LinkedListNode<int64>(num))
        length + 1, nextNode.Next, scores, nextPlayer


let (_,_,scores2,_) =
    lst <- LinkedList<int64>([0L])
    [1L..(int64 lastMarble)*100L]
    |> List.fold (doIterLL players) (0, lst.First, Map.empty, 1)

let ans2 =
    scores2
    |> Map.toArray
    |> Array.maxBy snd
    |> snd

