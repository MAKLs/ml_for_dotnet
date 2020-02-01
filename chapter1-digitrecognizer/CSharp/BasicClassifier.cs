using System.Collections.Generic;

public class BasicClassifier: IClassifier {
    private IEnumerable<Observation> data;
    private readonly IDistance distance;
    
    public BasicClassifier(IDistance distance) {
        this.distance = distance;
    }

    public void Train(IEnumerable<Observation> trainingSet) {
        this.data = trainingSet;
    }

    public string Predict(int[] pixels) {
        Observation best = null;
        double closest = double.MaxValue;

        foreach (var obs in this.data) {
            double dist = this.distance.Between(obs.Pixels, pixels);
            if (dist < closest) {
                closest = dist;
                best = obs;
            }
        }

        return best.Label;
    }
}