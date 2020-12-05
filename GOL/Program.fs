// Learn more about F# at http://fsharp.org

open System
open System.Threading


//Taken from https://fsharpforfunandprofit.com/posts/concurrency-reactive/
let createTimer interval eventHandler = 
    let timer = new System.Timers.Timer(float interval)
    timer.AutoReset <- true

    timer.Elapsed.Add eventHandler

    async {
        timer.Start()

        do! Async.Sleep 50000

        timer.Stop()
    
    }

let safeGetNeighVal (game: int[][]) row  col = 
    let numRows = game.Length 
    let numCols = if numRows = 0 then 0 else game.[0].Length 
    //printfn "rows: %d, cols %d" numRows numCols
    if row < numRows && row >= 0 && col < numCols && col >= 0 then 
        game.[row].[col] 
    else 
        0
    
let printBoard game = 
    let stringRow r = "[" + Array.fold (fun acc v -> acc + if v = 0 then "·" else "*") "" r  + "]"
    for row in game do
        printfn "%s" <| stringRow row
    //printfn "---------------"

let getNumLiveNeigh (game: int[][]) row col = 
    safeGetNeighVal game (row-1) (col-1) + 
    safeGetNeighVal game (row-1) (col) + 
    safeGetNeighVal game (row-1) (col+1) + 
    safeGetNeighVal game (row) (col-1) + 
    safeGetNeighVal game (row) (col+1) + 
    safeGetNeighVal game (row+1) (col-1) + 
    safeGetNeighVal game (row+1) (col) + 
    safeGetNeighVal game (row+1) (col+1) 

let livesNextGen curr numNeigh = 
    if (curr = 0 && numNeigh = 3) || (curr = 1 && (numNeigh = 2 || numNeigh = 3)) then
        1
    else
        0



let gameHandler (game: int[][]) = 
    Console.Clear()
    printBoard game
    let mutable newGame = Array.init game.Length (fun _ -> Array.init (game.[0].Length) (fun _ -> 0))
    //printfn "Old board had dim r:%d, c:%d" game.Length game.[0].Length
    //printfn "New board has dim r:%d, c:%d" newGame.Length newGame.[0].Length
    for i in 0 .. game.Length-1 do
        for j in 0 .. game.[0].Length-1 do
            //printfn "Setting row %d, col %d liveNeighs %d" i j <| getNumLiveNeigh game i j
            newGame.[i].[j] <- livesNextGen game.[i].[j] (getNumLiveNeigh game i j) 
    newGame


let insertGlider (game: int[][]) row col = 
    game.[row-1].[col] <- 1
    game.[row].[col+1] <- 1
    game.[row+1].[col-1] <- 1
    game.[row+1].[col] <- 1
    game.[row+1].[col+1] <- 1

let insertBlinker (game: int[][]) row col = 
    game.[row-1].[col] <- 1
    game.[row].[col] <- 1
    game.[row+1].[col] <- 1





[<EntryPoint>]
let main argv =
    let mutable i = 0
    let size = 25
    let mutable game =  Array.init size (fun _ -> Array.init size (fun _ -> 0))
    Console.Title <- "Yeet of Life"
    insertGlider game 1 1 
    insertBlinker game 2 12 
    insertBlinker game 12 2 
    let mainHandler _ = game <- gameHandler game
    let mainLoop = createTimer 300 mainHandler
    Async.RunSynchronously mainLoop
    
    0 
