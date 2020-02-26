#load "references.fsx"
open FSharp.Data
open System

type Titanic = CsvProvider<"titanic.csv">
type Passenger = Titanic.Row

let dataset = Titanic.GetSample ()

let mostFrequentLabelIn group =
    group
    |> Seq.countBy snd
    |> Seq.maxBy snd
    |> fst

let learn sample extractFeature extractLabel =
    let groups =
        sample
        |> Seq.map (fun obs -> extractFeature obs, extractLabel obs)
        |> Seq.groupBy fst
        |> Seq.map (fun (feature,group) -> feature, mostFrequentLabelIn group)
    let classifier obs =
        let featureVal = extractFeature obs
        groups
        |> Seq.find (fun (f,_) -> f = featureVal)
        |> snd
    classifier

let evaluate (observations:Passenger seq) classifier =
    observations
    |> Seq.averageBy (fun p -> if p.Survived = classifier p then 1.0 else 0.0)

let hasData extractFeature = extractFeature >> Option.isSome

let entropy label data =
    let size = data |> Seq.length
    data
    |> Seq.countBy label
    |> Seq.map (fun (_,count) -> float count / float size)
    |> Seq.sumBy (fun f -> if f > 0.0 then - f * log f else 0.0)

let splitEntropy extractLabel extractFeature data =
    let dataWithValues =
        data
        |> Seq.filter (extractFeature |> hasData)
    let size = dataWithValues |> Seq.length
    dataWithValues
    |> Seq.groupBy extractFeature
    |> Seq.sumBy (fun (_,group) ->
        let groupSize = group |> Seq.length
        let probaGroup = float groupSize / float size
        let groupEntropy = group |> entropy extractLabel
        probaGroup * groupEntropy
    )

let survived (p:Passenger) = p.Survived
let sex (p:Passenger) = Some(p.Sex)
let pclass (p:Passenger) = Some(p.Pclass |> string)
let port (p:Passenger) =
    if p.Embarked = ""
    then None
    else Some(p.Embarked)
let age (p:Passenger) =
    if Double.IsNaN(p.Age)
    then None
    elif p.Age < 7.0
    then Some("young")
    else Some("old")
let averageFare =
    dataset.Rows
    |> Seq.averageBy (fun p -> p.Fare)
let fareLevel (p:Passenger) =
    if p.Fare < averageFare
    then Some("cheap")
    else Some("expensive")

let features = 
    [
        "Sex", sex
        "Class", pclass
        "Port", port
        "Age", age
        "Fare", fareLevel
    ]

let sexClassifier = survived |> learn dataset.Rows sex

let classClassifier = survived |> learn dataset.Rows pclass


let fareClassifier = survived |> learn dataset.Rows fareLevel

let betterLearn sample extractFeature extractLabel =
    let branches =
        sample
        |> Seq.filter (extractFeature |> hasData)
        |> Seq.map (fun obs -> extractFeature obs |> Option.get, extractLabel obs)
        |> Seq.groupBy fst
        |> Seq.map (fun (feature,group) -> feature, mostFrequentLabelIn group)
        |> Map.ofSeq
    let labelForMissing =
        sample
        |> Seq.countBy extractLabel
        |> Seq.maxBy snd
        |> fst
    let classifier obs =
        let featureVal = extractFeature obs
        match featureVal with
        | None -> labelForMissing
        | Some(feature) ->
            match (branches.TryFind feature) with
            | None -> labelForMissing
            | Some(predictedLabel) -> predictedLabel
    classifier

let updatedClassifier = survived |> betterLearn dataset.Rows port

printfn "Compaison: most informative feature"
let h = dataset.Rows |> entropy survived
printfn "Base entropy: %.3f" h
dataset.Rows |> splitEntropy survived sex |> printfn "   Sex: %.3f"
dataset.Rows |> splitEntropy survived pclass |> printfn "   Class: %.3f"
dataset.Rows |> splitEntropy survived port |> printfn "   Port: %.3f"
dataset.Rows |> splitEntropy survived age |> printfn "   Age: %.3f"
dataset.Rows |> splitEntropy survived fareLevel |> printfn "   Fare: %.3f"