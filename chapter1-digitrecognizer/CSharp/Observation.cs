public class Observation {
    public string Label {get; set;}
    public int[] Pixels {get; set;}

    public Observation(string label, int[] pixels) {
        this.Label = label;
        this.Pixels = pixels;
    }
}