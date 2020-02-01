using System;
using System.Linq;
using System.IO;

public class DataReader {
    public static char Delimiter = ',';

    private static Observation ObservationFactory(string data) {
        string[] split_data = data.Split(DataReader.Delimiter);
        string label = split_data[0];
        int[] pixels = split_data
            .Skip(1)
            .Select(val => Convert.ToInt32(val))
            .ToArray();

        return new Observation(label, pixels);
    }

    public static Observation[] ReadObservations(string filePath) {
        Observation[] observations = File.ReadAllLines(filePath)
            .Skip(1)
            .Select(ObservationFactory)
            .ToArray();

        return observations;
    }
}