using System;

public class ManhattanDistance: IDistance {
    public double Between(int[] pixels1, int[] pixels2) {
        // Make sure the sample are the same size
        if (pixels1.Length != pixels2.Length) {
            throw new ArgumentException($"Samples must be same size: {pixels1.Length} != {pixels2.Length}");
        }

        int length = pixels1.Length;
        int distance = 0;

        for (int i = 0; i < length; i++) {
            distance += Math.Abs(pixels1[i] - pixels2[i]);
        }

        return distance;
    }
}