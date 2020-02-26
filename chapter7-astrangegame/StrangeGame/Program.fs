namespace Game

open System
open System.Threading
open Game
open Brains
open Rendering

module Program =

    // initialize world
    let size = { Width = 40; Height = 20 }
    let player = { Position = { Top = 10; Left = 20 }; Direction = North }

    let board = Array2D.init size.Width size.Height (fun _ _ ->
        rng.Next(tileValues.Length)
    )
    
    let score = 0
    let initialState = { Board = board; Hero = player; Score = score }

    [<EntryPoint>]
    let main argv =
        
        // game loop
        let rec loop (state:GameState,brain:Brain) =

            let currentState = visibleState size state.Board state.Hero
            let decision = decide brain currentState

            // world update
            let player = state.Hero |> applyDecision size decision
            let board = updateBoard state.Board player
            let gain = computeGain state.Board player
            let score = state.Score + gain
            let updatedWorld = { Board = board; Hero = player; Score = score }

            // render
            renderScore score
            render state updatedWorld

            // learning
            let nextState = visibleState size board player
            let experience = {
                State = currentState;
                Action = decision;
                Reward = float gain;
                NextState = nextState;
            }
            let brain = learn brain experience

            if Console.KeyAvailable
            then do
                let key = Console.ReadKey(false)
                if key.Key = ConsoleKey.P then renderBrain size brain
            Thread.Sleep 20
            loop (updatedWorld,brain)

        // start the game
        prepareDisplay size initialState
        ignore (loop (initialState,Map.empty))
        0 // return an integer exit code
