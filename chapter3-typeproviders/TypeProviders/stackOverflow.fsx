#load "references.fsx"
open FSharp.Data

[<Literal>]
let QuestionQuery = "https://api.stackexchange.com/2.2/questions?site=stackoverflow"

type Questions = JsonProvider<QuestionQuery>

let tagged tags query = 
    // join the tags together
    let joinedTags = tags |> String.concat ";"
    sprintf "%s&tagged=%s" query joinedTags

let page p query = sprintf "%s&page=%i" query p

let pageSize s query = sprintf "%s&pagesize=%i" query s

let extractQuestions (query:string) = Questions.Load(query).Items

let ``C#`` = "C%23"
let ``F#`` = "F%23"

let fsSample =
    QuestionQuery
    |> tagged [``F#``]
    |> pageSize 100
    |> extractQuestions
let csSample =
    QuestionQuery
    |> tagged [``C#``]
    |> pageSize 100
    |> extractQuestions

let analyzeTags (qs:Questions.Item seq) =
    qs
    |> Seq.collect (fun question -> question.Tags)
    |> Seq.countBy id
    |> Seq.filter (fun (_,count) -> count > 2)
    |> Seq.sortBy (fun (_,count) -> -count)
    |> Seq.iter (fun (tag,count) -> printfn "%s,%i" tag count)