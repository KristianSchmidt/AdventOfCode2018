#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let carToRoad =
    function
    | '<' | '>' -> '-'
    | '^' | 'v' -> '|'
    | x -> x

let data =
    Helpers.Web.getInput 13
    |> Array.mapi (fun x s -> s.ToCharArray() |> Array.mapi (fun y c -> ((x,y),c)))
    |> Array.collect id

let isCar c =
    c = '<' || c = '>' || c = '^' || c = 'v'

type Direction =
    | Up | Down | Left | Right

let carToDirection =
    function
    | '<' -> Left
    | '>' -> Right
    | '^' -> Up
    | 'v' -> Down

type Action = | TurnLeft | Straight | TurnRight

let nextAction = function | TurnLeft -> Straight | Straight -> TurnRight | TurnRight -> TurnLeft

type Car = { Pos : int*int; Direction : Direction; Action : Action }

let cars =
    data
    |> Array.filter (snd >> isCar)
    |> Array.map (fun (pos,sym) -> { Pos = pos; Direction = carToDirection sym; Action = TurnLeft })

let roads =
    data
    |> Array.map (fun (x,y) -> (x,carToRoad y))
    |> Map.ofArray

let newPos (x,y) dir =
    match dir with
    | Left -> (x,y-1)
    | Right -> (x,y+1)
    | Up -> (x-1,y)
    | Down -> (x+1,y)

let newDirection c dir =
    match c, dir with
    | '-', _ -> dir
    | '|', _ -> dir
    | '/', Left -> Down
    | '/', Up -> Right
    | '/', Down -> Left
    | '/', Right -> Up
    | '\\', Left -> Up
    | '\\', Up -> Left
    | '\\', Down -> Right
    | '\\', Right -> Down
    | _, _ -> failwithf "'%c' %A" c dir

let turn dir action =
    match dir, action with
    | _, Straight -> dir
    | Left, TurnLeft -> Down
    | Left, TurnRight -> Up
    | Right, TurnLeft -> Up
    | Right, TurnRight -> Down
    | Up, TurnLeft -> Left
    | Up, TurnRight -> Right
    | Down, TurnLeft -> Right
    | Down, TurnRight -> Left

let moveCar (car : Car) =
    let nextPos = newPos car.Pos car.Direction
    let prevTile = Map.find car.Pos roads
    let nextTile = Map.find nextPos roads
    let (nextDir, nextAct) = 
        match nextTile with
        | '+' -> turn car.Direction car.Action, nextAction car.Action
        | _ -> newDirection nextTile car.Direction, car.Action

    //printfn "Pos: %A NewPos: %A Prev: '%c' Next: '%c' OldDir: %A NewDir: %A" car.Pos nextPos prevTile nextTile car.Direction nextDir
    
    { Pos = nextPos; Direction = nextDir; Action = nextAct }

let dupes arr =
    let arr' =
        arr
        |> Array.countBy id |> Array.filter (snd >> (fun x -> x > 1))


    let maxCount = (arr |> Array.countBy id |> Array.map snd |> Array.max)
    if maxCount > 1 then
        printfn "%i" maxCount
    match arr' with
    | [||] -> None
    | [|pos|] -> Some (fst pos)
    | _ -> failwithf "x"

let run cars =
    let rec f cars =
        let newCars = cars |> Array.map moveCar
        match dupes (newCars |> Array.map (fun c -> c.Pos)) with
        | Some pos -> pos
        | None -> f newCars

    f cars

let ans1 =
    match run cars with
    | (x,y) -> sprintf "%i,%i" y x

// Part 2

let sortCars cars =
    cars |> Array.sortBy (fun (c : Car) -> 
                            let (row,col) = c.Pos
                            row*1000 + col)

let run2 cars =
    let rec f cars =
        let sortedCars = sortCars cars
        let mutable crashed = Set.empty
        for i in 0 .. sortedCars.Length-2 do
            if Set.contains i crashed |> not then
                let c = sortedCars[i]
                let nextPos = (moveCar c).Pos
                for j in i+1..sortedCars.Length-1 do
                    if nextPos = sortedCars[j].Pos then
                        crashed <- crashed |> Set.add i |> Set.add j
                    
        let cars' = sortedCars |> Array.mapi (fun i c -> if (Set.contains i crashed) then None else Some c) |> Array.choose id
        let newCars = cars' |> Array.map moveCar
        
        let positions = (newCars |> Array.map (fun c -> c.Pos))
        match Array.length positions, dupes positions with
        | 1, _ -> positions[0]
        | _, Some pos ->
            let newCars' =
                newCars
                |> Array.filter (fun c -> c.Pos <> pos)
            match newCars'.Length with
            | 1 -> newCars'[0].Pos
            | x -> f newCars'
        | _, None -> f newCars

    f cars

let ans2 =
    match run2 cars with
    | (x,y) -> sprintf "%i,%i" y x

