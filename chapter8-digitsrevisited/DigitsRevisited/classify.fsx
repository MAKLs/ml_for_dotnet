#load "references.fsx"
open System
open System.IO

type Observation = { Label:string; Pixels:int[] }
type Distance = int[] * int[] -> int
type Classifier = int[] -> string

let toObservation (csvData:string) =
    let columns = csvData.Split(',')
    let label = columns.[0]
    let pixels = columns.[1..] |> Array.map int
    { Label = label; Pixels = pixels }

let reader path =
    let data = File.ReadAllLines path
    data.[1..]
    |> Array.map toObservation

let trainingPath = __SOURCE_DIRECTORY__ + @"/data/trainingsample.csv"
let training = reader trainingPath

let euclidianDistance (pixels1,pixels2) =
    Array.zip pixels1 pixels2
    |> Array.sumBy (fun (x,y) -> pown (x - y) 2)

let d1 (pixels1,pixels2) =
    Array.zip pixels1 pixels2
    |> Array.sumBy (fun (x,y) -> (x - y) * (x - y))

let d2 (pixels1,pixels2) =
    (pixels1,pixels2)
    ||> Array.map2 (fun x y -> (x - y) * (x - y))
    |> Array.sum

let d3 (pixels1:int[],pixels2:int[]) =
    let dim = pixels1.Length
    let rec f acc i =
        if i = dim
        then acc
        else
            let x = pixels1.[i] - pixels2.[i]
            let acc' = acc + (x * x)
            f acc' (i + 1)
    f 0 0

let d4 (pixels1:int[],pixels2:int[]) =
    let dim = pixels1.Length
    let mutable dist = 0
    for i in 0 .. (dim - 1) do
        let x = pixels1.[i] - pixels2.[i]
        dist <- dist + (x * x)
    dist

let train (trainingSet:Observation[]) (dist:Distance) =
    let classify (pixels:int[]) =
        trainingSet
        |> Array.minBy (fun x -> dist (x.Pixels, pixels))
        |> fun x -> x.Label
    classify

let validationPath = __SOURCE_DIRECTORY__ + @"/data/validationsample.csv"
let validation = reader validationPath

let evaluate validationSet classifier =
    validationSet
    |> Array.averageBy (fun x -> if classifier x.Pixels = x.Label then 1.0 else 0.0)
    |> printfn "Correct: %.3f"

let parallelEvaluate validationSet classifier =
    validationSet
    |> Array.Parallel.map (fun x -> if classifier x.Pixels = x.Label then 1.0 else 0.0)
    |> Array.average
    |> printfn "Correct: %.3f"

let euclideanModel = train training euclidianDistance
let updatedModel = train training d4

// Profiling
#time "on"
