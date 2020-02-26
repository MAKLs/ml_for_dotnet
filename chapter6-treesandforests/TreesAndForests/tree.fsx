#load "references.fsx"
open System
open FSharp.Data
open Titanic.Tree

type Titanic = CsvProvider<"titanic.csv">
type Passenger = Titanic.Row

let dataset = Titanic.GetSample ()

let label (p:Passenger) = p.Survived

let features = [
    "sex", fun (p:Passenger) -> p.Sex |> Some
    "class", fun (p:Passenger) -> p.Pclass |> string |> Some
    "age", fun (p:Passenger) -> (if p.Age < 7.0 then "young" else "old") |> Some
]

let filters = [| entropyGainFilter; leafSizeFilter 10 |]
let tree = growTree filters dataset.Rows label (features |> Map.ofList)

// k-folds evaluation
let folds = dataset.Rows |> Seq.toArray |> kfold 10
let accuracy tree (sample:Passenger seq) =
    sample
    |> Seq.averageBy (fun p ->
        if p.Survived = decide tree p then 1.0 else 0.0
    )

let evaluateFolds =
    let features = features |> Map.ofList
    [ for (training,validation) in folds ->
        let tree = growTree filters training label features
        let accuracyTraining = accuracy tree training
        let accuracyValidation = accuracy tree validation

        printfn "Training: %.3f, Validation %.3f" accuracyTraining accuracyValidation
        accuracyTraining,accuracyValidation
    ]

let forestFeatures = [
    "sex", fun (p:Passenger) -> p.Sex |> Some
    "class", fun (p:Passenger) -> p.Pclass |> string |> Some
    "age", fun (p:Passenger) -> (if Double.IsNaN(p.Age) then None elif p.Age < 7.0 then Some("young") else Some("old"))
    "port", fun (p:Passenger) -> if p.Embarked = "" then None else Some(p.Embarked)
]
let forestResults () =
    let accuracy predictor (sample:Passenger seq) =
        sample
        |> Seq.averageBy (fun p ->
            if p.Survived = predictor p then 1.0 else 0.0
        )
    
    [ for (training,validation) in folds ->
        let forest = growForest 2000 filters training label forestFeatures

        let accuracyTraining = accuracy forest training
        let accuracyValidation = accuracy forest validation

        printfn "Training: %.3f, Validation: %.3f" accuracyTraining accuracyValidation
        accuracyTraining,accuracyValidation
    ]
