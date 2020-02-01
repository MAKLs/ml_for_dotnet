#load "references.fsx"
open System.IO

// Types
type Observation = { Label: string; Pixels: int[] }
type Distance = int[] * int[] -> int
type Model = int[] -> string

// Data processing
let toObservation (csvData:string) =
    let columns = csvData.Split(',')
    let label = columns.[0]
    let pixels = columns.[1..] |> Array.map int
    { Label = label; Pixels = pixels }

let reader path = 
    let data = File.ReadAllLines path
    data.[1..] |> Array.map toObservation

let toPercent f =
    100.0 * f

// Distance implementations
let manhattanDistance (pixels1, pixels2) =
    Array.zip pixels1 pixels2
    |> Array.sumBy (fun (p1, p2) -> abs(p1 - p2))

let euclideanDistance (pixels1, pixels2) =
    Array.zip pixels1 pixels2
    |> Array.sumBy  (fun (p1, p2) -> pown (p1 - p2) 2)

// Training data
printfn "Loading training data"
let trainingPath = @"data/trainingsample.csv"
let trainingData = reader trainingPath
// Validation data
printfn "Loading validation data"
let validationPath = @"data/validationsample.csv"
let validationData = reader validationPath

// Models
let train (trainingSet:Observation[]) (distance:Distance) =
    let classify (pixels:int[]) = 
        trainingSet
        |> Array.minBy (fun obs -> distance (obs.Pixels, pixels))
        |> fun x -> x.Label
    classify

let classifier = train trainingData

let manhattanClassifier = classifier manhattanDistance

let euclideanClassifier = classifier euclideanDistance

let validate (model:Model) (data:Observation[]) =
    data
    |> Array.averageBy (fun samp -> if model samp.Pixels = samp.Label then 1. else 0.)
    |> toPercent
    |> printfn "Correct %.2f%%"

// Validate the models
printfn "Validating models"
printfn "- Manhattan"
validate manhattanClassifier validationData
printfn "- Euclidean"
validate euclideanClassifier validationData