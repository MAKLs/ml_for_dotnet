#load "references.fsx"

open Accord.Statistics
open Accord.Neuro
open Accord.Neuro.Learning

let nnRead filename =
    let path = __SOURCE_DIRECTORY__ + @"/data/" + filename
    path
    |> System.IO.File.ReadAllLines
    |> fun lines -> lines.[1..]
    |> Array.map (fun line ->
        let parsed = line.Split ','
        parsed.[0] |> int, parsed.[1..] |> Array.map float
    )

let trainNetwork (epochs:int) =
    let features = 28 * 28
    let labels,images = nnRead "trainingsample.csv" |> Array.unzip
    let learningLabels = Tools.Expand(labels, -1.0, 1.0)

    let network = ActivationNetwork(BipolarSigmoidFunction(), features, [| 128; 64; 10; 10 |])
    NguyenWidrow(network).Randomize()

    let teacher = new ParallelResilientBackpropagationLearning(network)

    let rec learn iter prevError =
        let error = teacher.RunEpoch(images, learningLabels)
        printfn "%.3f / %i" error iter
        if abs (error - prevError) < 0.01 then ignore ()
        elif iter > epochs then ignore ()
        else learn (iter + 1) error
    
    learn 0 0.

    network

let ann = trainNetwork 500

let predict (model: ActivationNetwork) (image:float[]) =
    model.Compute image
    |> Array.mapi (fun i x -> i,x)
    |> Array.maxBy snd
    |> fst

let saveModel (model: ActivationNetwork) path =
    let fs = System.IO.File.OpenWrite(path)
    model.Save(fs)
    fs.Close()

let validate = nnRead "validationsample.csv"
validate
|> Array.averageBy (fun (label,image) ->
    let predicted = predict ann image
    if label = predicted then 1. else 0.
)
