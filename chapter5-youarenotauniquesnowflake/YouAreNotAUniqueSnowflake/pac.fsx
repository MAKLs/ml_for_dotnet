#load "references.fsx"
open System.IO
open MathNet
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open Unsupervised.PAC
open XPlot.Plotly

// Load data
let folder = __SOURCE_DIRECTORY__
let file = "test.csv"

let headers,observations =
    let raw =
        folder + "/" + file
        |> File.ReadAllLines
    let headers = (raw.[0].Split ',').[1..]
    let observations =
        raw.[1..]
        |> Array.map (fun line -> (line.Split ',').[1..])
        |> Array.map (Array.map float)
    headers,observations

let correlations =
    observations
    |> Matrix.Build.DenseOfColumnArrays
    |> Matrix.toRowArrays
    |> Correlation.PearsonMatrix

let features = headers.Length
[
    for col in 0 .. (features - 1) do
        for row in (col + 1) .. (features - 1) ->
            correlations.[col,row], headers.[col], headers.[row]
]
|> Seq.sortByDescending (fun (corr, f1, f2) -> abs corr)
|> Seq.take 20
|> Seq.iter (fun (corr, f1, f2) ->
    printfn "%s %s: %.2f" f1 f2 corr
)

let normalized = normalize (headers.Length) observations

let (eValues,eVectors), projector = pca normalized

let total = eValues |> Seq.sumBy (fun e -> e.Magnitude)
eValues
|> Vector.toList
|> List.rev
|> List.scan (fun (percent,cumul) value ->
    let percent = 100.0 * value.Magnitude / total
    let cumul = cumul + percent
    (percent,cumul)) (0.0,0.0)
|> List.tail
|> List.iteri (fun i (p,c) -> printfn "Feature %2i: %.2f%% (%.2f%%)" i p c)

let principalComponent comp1 comp2 =
    let title = sprintf "Component %i vs %i" comp1 comp2
    let features = headers.Length
    let options =
        Options(
            title=title,
            xaxis=Xaxis(title=(sprintf "Component %i" comp1), range=[-1;1]),
            yaxis=Yaxis(title=(sprintf "Component %i" comp2), range=[-1;1]),
            height=800.0,
            width=1200.0,
            showlegend=false
        )
    let compSeries =
        Seq.zip (eVectors.Column(features - comp1)) (eVectors.Column(features - comp2))
        |> Seq.mapi (fun i (c1,c2) ->
            Scatter(x=[c1], y=[c2], mode="markers+text", text=headers.[i])
        )
    compSeries
    |> Chart.Plot
    |> Chart.WithOptions options
    |> Chart.Show

let projections comp1 comp2 =
    let title = sprintf "Component %i vs %i" comp1 comp2
    let features = headers.Length
    let options =
        Options(
            title=title,
            xaxis=Xaxis(title=(sprintf "Component %i" comp1)),
            yaxis=Yaxis(title=(sprintf "Component %i" comp2)),
            height=800.0,
            width=1200.0,
            showlegend=false
        )
    let compSeries =
        normalized
        |> Seq.map (projector >> (fun (obs:float[]) -> obs.[features - comp1], obs.[features - comp2]))
        |> Seq.mapi (fun i (c1,c2) ->
            Scatter(x=[c1], y=[c2], mode="markers")
        )
    compSeries
    |> Chart.Plot
    |> Chart.WithOptions options
    |> Chart.Show