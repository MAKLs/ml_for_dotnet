namespace Titanic

module Tree =

    open System
    
    type Feature<'a> = 'a -> string Option
    type NamedFeature<'a> = string * Feature<'a>
    type Label<'a,'b when 'b:equality> = 'a -> 'b

    type Tree<'a,'b when 'b:equality> =
        | Answer of 'b
        | Stump of NamedFeature<'a> * string * Map<string,Tree<'a,'b>>

    let mostFrequentBy f sample =
        sample
        |> Seq.map f
        |> Seq.countBy id
        |> Seq.maxBy snd
        |> fst
    
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

    let rec decide tree observation =
        match tree with
        | Answer(labelVal) -> labelVal
        | Stump((featureName,feature),defaultVal,branches) ->
            let featureVal = feature observation
            let usedVal =
                match featureVal with
                | None -> defaultVal
                | Some(value) ->
                    match (branches.TryFind value) with
                    | None -> defaultVal
                    | Some(_) -> value
            let nextTree = branches.[usedVal]
            decide nextTree observation
    
    let entropyGainFilter sample label feature =
        // Information gain must be strictly positive
        splitEntropy label feature sample - entropy label sample < 0.0
    
    let leafSizeFilter minSize sample label feature =
        sample
        |> Seq.map feature
        |> Seq.choose id
        |> Seq.countBy id
        |> Seq.forall (fun (_,groupSize) -> groupSize > minSize)
    
    let rec growTree filters sample label features =
        // Filter our features
        let features =
            features
            |> Map.filter (fun name feature ->
                filters |> Seq.forall (fun filter -> filter sample label feature)
            )
        if (Map.isEmpty features)
        // No more features to split on so prediction is most frequent label
        then sample |> mostFrequentBy label |> Answer
        else
            // get the named feature with the largest entropy gain
            let (bestName,bestFeature) =
                features
                |> Seq.minBy (fun kv ->
                    splitEntropy label kv.Value sample
                )
                |> (fun kv -> kv.Key,kv.Value)
            let branches =
                sample
                |> Seq.groupBy bestFeature
                |> Seq.filter (fun (value,group) -> value.IsSome)
                |> Seq.map (fun (value,group) -> value.Value,group)
            // find most frequent value for feature to use as default for missing values
            let defaultVal =
                branches
                |> Seq.maxBy (fun (value,group) ->
                    group |> Seq.length
                )
                |> fst
            // remove the best feature from canidates for later branches in tree
            let remainingFeatures = features |> Map.remove bestName
            // grow the next level of the tree
            let nextLevel =
                branches
                |> Seq.map (fun (value,group) ->
                    value, growTree filters group label remainingFeatures
                )
                |> Map.ofSeq
            
            Stump((bestName,bestFeature),defaultVal,nextLevel)
    
    let rec displayTree depth tree =
        let padding = String.replicate (2 * depth) " "
        match tree with
        | Answer(label) -> printfn " -> %A" label
        | Stump((name,_),_,branches) ->
            printfn ""
            branches
            |> Seq.iter (fun kv ->
                printf "%s ? %s : %s" padding name kv.Key
                displayTree (depth + 1) kv.Value
            )
    
    let kfold k sample =
        let size = sample |> Array.length
        let foldSize = size / k
        [ for f in 0 .. (k - 1) do
            let sliceStart = f * foldSize
            let sliceEnd = f * foldSize + foldSize - 1
            let validation = sample.[sliceStart .. sliceEnd]
            let training =
                [|
                    for i in 0 .. (sliceStart - 1) do yield sample.[i]
                    for i in (sliceEnd + 1) .. (size - 1) do yield sample.[i]
                |]
            yield training,validation
        ]
    
    let pickRepeat (rng:Random) proportion original =
        let size = original |> Array.length
        let sampleSize = proportion * (float size) |> int
        Array.init sampleSize (fun _ -> original.[rng.Next(size)])

    let pickNoRepeat (rng:Random) proportion original =
        let size = original |> Seq.length
        let sampleSize = proportion * (float size) |> int

        let init = ([],size)
        original
        |> Seq.fold (fun (sampled,remaining) item ->
            let picked = List.length sampled
            let p = float (sampleSize - picked) / float remaining
            if (rng.NextDouble () <= p)
            then (item::sampled,remaining-1)
            else (sampled,remaining-1)
        ) init
        |> fst
    
    let predict forest observation =
        forest
        |> Seq.map (fun tree -> decide tree observation)
        |> Seq.countBy id
        |> Seq.maxBy snd
        |> fst
    
    let growForest size filters sample label features =
        let rng = Random ()

        let propFeatures =
            let total = features |> Seq.length |> float
            sqrt total / total
        
        let featSample () = pickNoRepeat rng propFeatures features
        let popSample () = pickRepeat rng 1.0 sample
        
        let forest = [
            for _ in 1 .. size ->
                let sample = popSample ()
                let features = featSample () |> Map.ofList
                growTree filters sample label features
        ]

        let predictor = predict forest
        predictor