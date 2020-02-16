namespace Unsupervised

module KMeans =

    let pickFrom size k =
        let rng = System.Random()
        let rec pick (set:int Set) =
            let candidate = rng.Next(size)
            let set = set.Add candidate
            if set.Count = k then set
            else pick set
        pick Set.empty |> Set.toArray
    
    let initialize observations k =
        let size = Array.length observations
        let centroids =
            pickFrom size k
            |> Array.mapi (fun i index ->
                i+1, observations.[index]
            )
        let assignments =
            observations
            |> Array.map (fun o -> 0,o)
        (assignments,centroids)
    
    let clusterize distance centroidOf observations k =
        let rec search (assignments,centroids) =
            // ID of closest centroid
            let classifier observation =
                centroids
                |> Array.minBy (fun (_,centroid) ->
                    distance observation centroid
                )
                |> fst
            // Assign each observation to closest centroid
            let assignments' =
                assignments
                |> Array.map (fun (_,observation) ->
                    let closestCentroid = classifier observation
                    (closestCentroid,observation)
                )
            // Check if any cluster-changes occurred
            let changed =
                (assignments,assignments')
                ||> Seq.zip
                |> Seq.exists (fun ((oldCluster,_),(newCluster,_)) ->
                    oldCluster <> newCluster
                )
        
            if changed
            then
                let centroids' =
                    assignments'
                    |> Seq.groupBy fst
                    |> Seq.map (fun (clusterId,group) ->
                        clusterId, group |> Seq.map snd |> centroidOf
                    )
                    |> Seq.toArray
                search (assignments',centroids')
            else centroids,classifier
    
        // Start search from initial values
        let initialCentroids = initialize observations k
        search initialCentroids
