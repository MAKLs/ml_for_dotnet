namespace Game

open System
open Game
open Brains

module Rendering =

    let colors = [|
        ConsoleColor.DarkRed
        ConsoleColor.Yellow
        ConsoleColor.DarkBlue
        ConsoleColor.Blue
    |]
    let creatureColor = ConsoleColor.White

    let offset (pos:Pos) = (pos.Left, pos.Top + 2)
    let writeAt (left,top) color (text:string) =
        Console.ForegroundColor <- color
        Console.SetCursorPosition (left,top)
        Console.Write text
    
    let prepareDisplay size gameState =
        // Console.SetWindowSize (size.Width, size.Height)
        Console.Clear ()
        Console.CursorVisible <- false
        let board = gameState.Board
        for x in 0 .. (size.Width - 1) do
            for y in 0 .. (size.Height - 1) do
                let pos = { Left = x; Top = y }
                Console.SetCursorPosition (offset pos)
                let tileType = board.[x,y]
                Console.BackgroundColor <- colors.[tileType]
                Console.Write(" ")

    let render (before:GameState) (after:GameState) =
        let oldPos = before.Hero.Position
        let newPos = after.Hero.Position
        // previous player position
        let tileType = after.Board.[oldPos.Left,oldPos.Top]
        Console.SetCursorPosition (offset oldPos)
        Console.BackgroundColor <- colors.[tileType]
        Console.Write(" ")
        // current position
        Console.SetCursorPosition (offset newPos)
        Console.BackgroundColor <- creatureColor
        Console.Write(" ")
    
    let renderScore score =
        Console.SetCursorPosition (0,0)
        Console.BackgroundColor <- ConsoleColor.Black
        Console.ForegroundColor <- ConsoleColor.White
        printfn "Score: %i    " score
    
    let renderState (pos:Pos) (state:State) =
        state
        |> Seq.zip offsets
        |> Seq.iter (fun ((x,y),cellType) ->
            let cellPos = { Left = pos.Left + x; Top = pos.Top + y }
            Console.SetCursorPosition (cellPos.Left,cellPos.Top)
            Console.BackgroundColor <- colors.[cellType]
            Console.Write(" ")
        )
    
    let renderBrain size (brain:Brain) =
        Console.Clear ()
        let mutable pos = { Left = size.Width + 4; Top = 3 }
        let stateGroups =
            brain
            |> Map.toSeq
            |> Seq.groupBy (fun (strat,_) ->
                strat.State
            )
            |> Seq.sortByDescending (fun (_,strats) -> strats |> Seq.length)
            |> Seq.take 7
        for (state,strategies) in stateGroups do
            renderState pos state
            for (strat,reward) in strategies do
                Console.SetCursorPosition (pos.Left + 4, pos.Top - 1)
                Console.BackgroundColor <- ConsoleColor.Black
                Console.ForegroundColor <- ConsoleColor.White
                printfn "Action: %A Reward: %.2f" strat.Action reward
                pos <- { Left = pos.Left; Top = pos.Top + 1 }
            pos <- { Left = pos.Left; Top = pos.Top + 3 }

        // Console.SetCursorPosition (offset pos)
        // Console.BackgroundColor <- ConsoleColor.Black
        // Console.ForegroundColor <- ConsoleColor.White
        // printfn "%A" (brain |> Map.toSeq |> Seq.groupBy (fun (strat,gain) -> strat.State))
    
    