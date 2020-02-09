#load "references.fsx"
open System
open FSharp.Data
open XPlot.GoogleCharts

[<Literal>]
let DayData = __SOURCE_DIRECTORY__ + "/../data/day.csv"

type Data = CsvProvider<DayData>
type Obs = Data.Row
type Model = Obs -> float

// Data
let dataSet = Data.Load(DayData)
let data = dataSet.Rows

// Plots
let ma n (series:float seq) =
    series
    |> Seq.windowed n
    |> Seq.map (fun chunk -> chunk |> Seq.average)
    |> Seq.toList

let all = [for obs in data -> float obs.Cnt]
// let instants = [for obs in data -> float obs.Instant]
// let inputs = [
//     instants, all
//     instants |> List.toSeq |> Seq.windowed 7 |> Seq.map (fun w -> w |> Seq.last) |> Seq.toList, ma 7 all
//     instants |> List.toSeq |> Seq.windowed 30 |> Seq.map (fun w -> w |> Seq.last) |> Seq.toList, ma 30 all
// ]

// Model
let baseline =
    let avg = data |> Seq.averageBy (fun sample -> float sample.Cnt)
    data |> Seq.averageBy (fun sample -> abs (float sample.Cnt - avg))

let model (theta0, theta1) (obs:Obs) =
    theta0 + theta1 * (float obs.Instant)

let model0 = model (4504.0, 0.0)
let model1 = model (6000.0, -4.5)

let cost (data:Obs seq) (m:Model) =
    data
    |> Seq.sumBy (fun o -> pown (float o.Cnt - m o) 2)
    |> sqrt
let overallCost = cost data
overallCost model0 |> printfn "Cost model0: %.0f"
overallCost model1 |> printfn "Cost model1: %.0f"

let update alpha (theta0, theta1) (obs:Obs) =
    let y = float obs.Cnt
    let x = float obs.Instant
    let theta0' = theta0 - 2.0 * alpha * 1.0 * (theta0 + theta1 * x - y)
    let theta1' = theta1 - 2.0 * alpha * x * (theta0 + theta1 * x - y)
    theta0', theta1'

let batchUpdate rate (theta0, theta1) (data:Obs seq) =
    let updates =
        data
        |> Seq.map (update rate (theta0, theta1))
    let theta0' = updates |> Seq.averageBy fst
    let theta1' = updates |> Seq.averageBy snd
    theta0',theta1'

let stochastic rate (theta0, theta1) =
    data
    |> Seq.fold (fun (t0, t1) obs ->
        printfn "%.4f,%.4f\n" t0 t1
        update rate (t0,t1) obs) (theta0, theta1)

let batch rate iters =
    let rec search (t0,t1) i =
        if i = 0 then (t0,t1)
        else
            search (batchUpdate rate (t0,t1) data) (i-1)
    search (0.,0.) iters

let bestRate =
    [for r in 1..20 ->
        (pown 0.1 r), stochastic (pown 0.1 r) (0., 0.) |> model |> overallCost]
    |> Seq.filter (fun (_,cost) -> not (Double.IsNaN(cost)))
    |> Seq.minBy (fun (_,cost) -> cost)
    |> fst

let model2 = model (stochastic bestRate (0., 0.))
let model3 = model (batch 0.000001 100)

let hiRate = 10. * bestRate
let errors =
    data
    |> Seq.scan (fun (t0,t1) obs -> update hiRate (t0,t1) obs) (0., 0.)
    |> Seq.map (model >> overallCost)

let batchedError rate =
    Seq.unfold (fun (t0,t1) ->
        let (t0',t1') = batchUpdate rate (t0,t1) data
        let err = model (t0,t1) |> overallCost
        Some(err, (t0',t1'))) (0.,0.)
    |> Seq.take 100

// Plotting
let inputs = [
    data |> Seq.map (fun o -> float o.Instant, float o.Cnt)
    data |> Seq.map (fun o -> float o.Instant, model0 o)
    data |> Seq.map (fun o -> float o.Instant, model1 o)
    data |> Seq.map (fun o -> float o.Instant, model2 o)
    data |> Seq.map (fun o -> float o.Instant, model3 o)
]

errors |> Chart.Line |> Chart.Show
batchedError 0.000001 |> Chart.Line |> Chart.Show

inputs
|> Chart.Combo
|> Chart.WithOptions
    (Options(title = "Bike riders"))
|> Chart.WithLabels ["Raw data"; "Model 0"; "Model 1"; "Model 2"]
|> Chart.WithLegend true
|> Chart.WithSize (2000, 800)
|> Chart.Show