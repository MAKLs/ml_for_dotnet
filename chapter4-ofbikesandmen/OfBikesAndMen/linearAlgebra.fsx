#load "references.fsx"
open FSharp.Data
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open XPlot.GoogleCharts

let seed = 314159
let rng = System.Random(seed)

[<Literal>]
let DayData = __SOURCE_DIRECTORY__ + "/../data/day.csv"

type Data = CsvProvider<DayData>
let dataSet = Data.Load(DayData)
let data = dataSet.Rows

type Vec = Vector<float>
type Mat = Matrix<float>

type Obs = Data.Row
type Model = Obs -> float
type Featurizer = Obs -> float list

let cost (theta:Vec) (Y:Vec) (X:Mat) =
    let ps = Y - (theta * X.Transpose())
    ps * ps |> sqrt

let predict (theta:Vec) (v:Vec) = theta * v

let X = matrix [for obs in data -> [1.; float obs.Instant]]
let Y = vector [for obs in data -> float obs.Cnt]

let estimate (Y:Vec) (X:Mat) =
    (X.Transpose() * X).Inverse() * X.Transpose() * Y

// Fisher-Yates shuffle
let shuffle (arr:'a []) =
    let arr = Array.copy arr
    let l = arr.Length
    for i in (l - 1) .. -1 .. 1 do
        let temp = arr.[i]
        let j = rng.Next(0, i + 1)
        arr.[i] <- arr.[j]
        arr.[j] <- temp
    arr

let training,validation =
    let shuffled =
        data
        |> Seq.toArray
        |> shuffle
    let size =
        0.7 * float (Array.length shuffled) |> int
    shuffled.[..size], shuffled.[size + 1 ..]

let predictor (f:Featurizer) (theta:Vec) =
    f >> vector >> (*) theta

let evaluate (model:Model) (data:Obs seq) =
    data
    |> Seq.averageBy (fun obs -> abs (model obs - float obs.Cnt))

let model (f:Featurizer) (data:Obs seq) =
    let Yt, Xt =
        data
        |> Seq.toList
        |> List.map (fun obs -> float obs.Cnt, f obs)
        |> List.unzip
    let theta = estimate (vector Yt) (matrix Xt)
    let predict = predictor f theta
    theta, predict

// Models
let featurizer0 (obs:Obs) =
    [ 1.0; float obs.Instant ]
let (theta0,model0) = model featurizer0 training

let featurizer1 (obs:Obs) =
    [   1.
        obs.Instant |> float
        obs.Atemp |> float
        obs.Hum |> float
        obs.Temp |> float
        obs.Windspeed |> float
    ]
let (theta1,model1) = model featurizer1 training

let featurizer2 (obs:Obs) =
    [   1.
        obs.Instant |> float
        obs.Atemp |> float
        obs.Hum |> float
        obs.Temp |> float
        obs.Windspeed |> float
        if obs.Holiday then 1.0 else 0.0
        if obs.Weathersit = 1 then 1.0 else 0.0
        if obs.Weathersit = 2 then 1.0 else 0.0
        if obs.Season = 2 then 1.0 else 0.0
        if obs.Season = 3 then 1.0 else 0.0
        if obs.Season = 4 then 1.0 else 0.0
        if obs.Weekday = 1 then 1.0 else 0.0
        if obs.Weekday = 2 then 1.0 else 0.0
        if obs.Weekday = 3 then 1.0 else 0.0
        if obs.Weekday = 4 then 1.0 else 0.0
        if obs.Weekday = 5 then 1.0 else 0.0
        if obs.Weekday = 6 then 1.0 else 0.0
    ]
let (theta2,model2) = model featurizer2 training

let squareTempFeaturizer (obs:Obs) =
    [   1.
        obs.Temp |> float
        obs.Temp * obs.Temp |> float
    ]
let (_,squareTempModel) = model squareTempFeaturizer data

let featurizer3 (obs:Obs) =
    [   1.
        obs.Instant |> float
        obs.Atemp |> float
        obs.Hum |> float
        obs.Temp |> float
        obs.Windspeed |> float
        obs.Temp * obs.Temp |> float
        obs.Atemp * obs.Atemp |> float
        if obs.Holiday then 1.0 else 0.0
        if obs.Weathersit = 1 then 1.0 else 0.0
        if obs.Weathersit = 2 then 1.0 else 0.0
        if obs.Season = 2 then 1.0 else 0.0
        if obs.Season = 3 then 1.0 else 0.0
        if obs.Season = 4 then 1.0 else 0.0
        if obs.Weekday = 1 then 1.0 else 0.0
        if obs.Weekday = 2 then 1.0 else 0.0
        if obs.Weekday = 3 then 1.0 else 0.0
        if obs.Weekday = 4 then 1.0 else 0.0
        if obs.Weekday = 5 then 1.0 else 0.0
        if obs.Weekday = 6 then 1.0 else 0.0
    ]
let (theta3,model3) = model featurizer3 training

// Plotting
let plots = [ 
    data |> Seq.map (fun o -> float o.Instant, float o.Cnt)
    data |> Seq.map (fun o -> float o.Instant, model0 o)
    data |> Seq.map (fun o -> float o.Instant, model1 o)
    data |> Seq.map (fun o -> float o.Instant, model2 o)
    data |> Seq.map (fun o -> float o.Instant, model3 o)
]

plots
|> Chart.Combo
|> Chart.WithLabels ["Raw data"; "Model 0"; "Model 1"; "Model 2"; "Model 3"]
|> Chart.WithLegend true
|> Chart.WithSize (2000, 800)
|> Chart.Show
