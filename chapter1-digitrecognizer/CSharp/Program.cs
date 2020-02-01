using System;

namespace OcrCSharp
{
    class Program
    {
        static void Main(string[] args)
        {
            // Classifier setup
            var distance = new ManhattanDistance();
            var classifier = new BasicClassifier(distance);

            // Training
            string dataPath = @"../data/trainingsample.csv";
            Console.WriteLine($"Reading training data from {dataPath}");
            Observation[] trainingSet = DataReader.ReadObservations(dataPath);
            classifier.Train(trainingSet);

            // Validation
            string validationPath = @"../data/validationsample.csv";
            Console.WriteLine($"Reading validation data from {validationPath}");
            Observation[] validationSet = DataReader.ReadObservations(validationPath);
            Console.WriteLine("Validating the classifier...");
            var correct = Evaluator.Correct(validationSet, classifier);
            Console.WriteLine("Classification score: {0:P2}", correct);
            
            Console.ReadLine();
        }
    }
}
