#load "references.fsx"

open Game.Game
open Game.Brains

let size = { Width = 40; Height = 20 }
let player = { Position = { Top = 10; Left = 20}; Direction = North }

let board = Array2D.init size.Width size.Height (fun _ _ ->
    rng.Next(tileValues.Length)
)


let score = 0
let initialState = { Board = board; Hero = player; Score = score }

let simulate (decide:Brain -> State -> Act) iters runs =

    let rec loop (state:GameState,brain:Brain,iter:int) =
        let currentState = visibleState size state.Board state.Hero
        let decision = decide brain currentState

        // update
        let player = state.Hero |> applyDecision size decision
        let board = updateBoard state.Board player
        let gain = computeGain state.Board player
        let score = state.Score + gain

        // learning
        let nextState = visibleState size board player
        let experience = {
            State = currentState;
            Action = decision;
            Reward = float gain;
            NextState = nextState;
        }
        let brain = learn brain experience

        let updated = { Board = board; Hero = player; Score = score }

        if iter < iters
        then loop (updated,brain,iter+1)
        else score
    
    [ for _ in 1 .. runs -> loop (initialState,Map.empty,0) ]

// Evaluation
let iterations = 100000
let runs = 20

printfn "Evaluating crude learning..."
let crude = simulate Game.Brains.decide iterations runs
printfn "Average score: %.0f" (crude |> Seq.averageBy float)