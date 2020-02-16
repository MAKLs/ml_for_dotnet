#load "references.fsx"
open System
open System.IO
open XPlot.Plotly
open Unsupervised.KMeans

let folder = __SOURCE_DIRECTORY__
let file = "oldData.csv"

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

printfn "%16s %8s %8s %8s" "Tag Name" "Avg" "Min" "Max"

headers
|> Array.mapi (fun i name ->
    let col = observations |> Array.map (fun obs -> obs.[i])
    let avg = col |> Array.average
    let min = col |> Array.min
    let max = col |> Array.max
    printfn "%16s %8.1f %8.1f %8.1f" name avg min max
)

let traces =
    headers
    |> Seq.mapi (fun i name ->
        name, observations
        |> Seq.averageBy (fun obs -> obs.[i])
    )

// Clustering
type Observation = float []

let features = headers.Length

let distance (obs1:Observation) (obs2:Observation) =
    (obs1,obs2)
    ||> Seq.map2 (fun o1 o2 -> pown (o1 - o2) 2)
    |> Seq.sum

let centroidOf (cluster:Observation seq) =
    Array.init features (fun f ->
        cluster
        |> Seq.averageBy (fun u -> u.[f])
    )

// Examples
let observations1 =
    observations
    |> Array.map (Array.map float)
    |> Array.filter (fun o -> Array.sum o > 0.0)

let (clusters1,classifier1) =
    let clustering = clusterize distance centroidOf
    let k = 5
    clustering observations1 k

clusters1
|> Seq.iter (fun (id,profile) ->
    printfn "CLUSTER %i" id
    profile
    |> Array.iteri (fun i value -> printfn "%16s %.1f" headers.[i] value)
)

let rowNormalizer (obs:Observation): Observation =
    let max = obs |> Seq.max
    obs |> Array.map (fun o -> o / max)

let observations2 =
    observations
    |> Array.map (Array.map float)
    |> Array.filter (fun o -> Array.sum o > 0.0)
    |> Array.map rowNormalizer

let (clusters2,classifier2) =
    let clustering = clusterize distance centroidOf
    let k = 5
    clustering observations2 k

// Determining k
let ruleOfThumb (n:int) = sqrt (float n / 2.0)
let kRuleOfThumb = ruleOfThumb (observations2.Length)

let squareError (obs1:Observation) (obs2:Observation) =
    (obs1,obs2)
    ||> Seq.zip
    |> Seq.sumBy (fun (o1,o2) -> pown (o1 - o2) 2)

let RSS (dataset:Observation []) centroids =
    dataset
    |> Seq.sumBy (fun obs ->
        centroids
        |> Seq.map (squareError obs)
        |> Seq.min
    )

let AIC (dataset:Observation []) centroids =
    let k = centroids |> Seq.length
    let m = dataset.[0] |> Seq.length
    RSS dataset centroids + float (2 * m * k)

let (bestClusters,bestClassifier) =
    let clustering = clusterize distance centroidOf
    let k = 10
    seq {
        for _ in 1 .. 20 ->
            clustering observations2 k
    }
    |> Seq.minBy (fun (cs,f) ->
        RSS observations2 (cs |> Seq.map snd)
    )

bestClusters
|> Seq.iter (fun (id,profile) ->
    printfn "CLUSTER %i" id
    profile
    |> Array.iteri (fun i value ->
        if value > 0.2 then printfn "%16s %.1f" headers.[i] value
    )
    printfn "\n"
)

// Plotting
let plotClusters (clusters:(int * Observation) []) =
    [ for (_,profile) in clusters ->
        profile
        |> Seq.mapi (fun i value -> headers.[i], value)
    ]
    |> Chart.Bar
    |> Chart.WithSize (1500,900)
    |> Chart.WithLabels [for i in 1..(Seq.length clusters) -> sprintf "cluster %i" i]
    |> Chart.Show

let testAIC ki ke samples (dataset:Observation []) =
    [ki .. ke]
    |> Seq.map (fun k ->
        let value =
            [ for _ in 1 .. samples ->
                let (clusters, classifier) =
                    let clustering = clusterize distance centroidOf 
                    clustering dataset k
                AIC dataset (clusters |> Seq.map snd) ]
            |> List.average  
        k, value
    )