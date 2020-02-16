#load "references.fsx"
open FSharp.Data
open FSharp.Core
open System.IO
open System
open System.Threading
open Deedle

[<Literal>]
let BaseQuery = """https://api.stackexchange.com/2.2"""

[<Literal>]
let TagBase = BaseQuery + """/tags"""

[<Literal>]
let UserBase = BaseQuery + """/users"""

[<Literal>]
let UserSample = TagBase + """/python/top-answerers/month?site=stackoverflow"""

[<Literal>]
let TagSample = TagBase + """?site=stackoverflow"""

type Builder = string -> string

type UserType =
    | Asker
    | Answerer

type Period =
    | All
    | Month

type Tags = JsonProvider<"samples/tagswithuser.json">
type Users = JsonProvider<"samples/users.json">


// Argument builders
let sorted on query =
    sprintf "%s&sort=%s" query on

let sized s query =
    sprintf "%s&pagesize=%i" query s

let ordered dir query =
    sprintf "%s&order=%s" query dir

let maxSize m query =
    sprintf "%s&max=%i" query m

let topUsersByTag (user:UserType) (period:Period) tag =
    let cleanChars = ["#", "%23"] |> Map.ofSeq;
    let userStr = if user = Asker then "top-askers" else "top-answerers"
    let periodStr = if period = All then "all_time" else "month"
    let tag = cleanChars |> Map.fold (fun (s:string) k v -> s.Replace(k, v)) tag
    TagBase + (sprintf "/%s/%s/%s" tag userStr periodStr)

let tagsByUser (userIds:int seq) =
    let idStr =
        userIds
        |> Seq.map (fun chunk -> sprintf "%i" chunk)
        |> String.concat ";"
    UserBase + (sprintf "/%s/tags" idStr)

let noopBuilder (s:string) = s

let buildQuery query (builder:Builder) =
    sprintf "%s?site=stackoverflow" query
    |> builder

let frameData (userIds:int seq) (tags:Tags.Item seq) (popTags:Tags.Item seq) =
    let df: Frame<int, string> = frame []
    df.AddColumn("UserId", userIds)
    let groupedTags = tags |> Seq.groupBy (fun tag -> tag.Name)
    popTags
    |> Seq.iter (fun tag ->
        let tagGroup =
            groupedTags
            |> Seq.tryFind (fun (tagname,group) -> tagname = tag.Name)
        let count = 
            match tagGroup with
            | None -> 0 |> Seq.replicate (Seq.length userIds)
            | Some((tagname,group)) ->
                userIds
                |> Seq.map (fun uid -> 
                    let userScore =
                        group
                        |> Seq.tryFind (fun score -> score.UserId = uid)
                    match userScore with
                    | None -> 0
                    | Some(s) -> s.Count
                )
        df.AddColumn(tag.Name, count)
    )

    df

let writeData (path:string) (df:Frame<int, string>) =
    df.SaveCsv(path, includeRowKeys=false)

// Extract data
let popularTags =
    let builder = sorted "popular" >> sized 30 >> ordered "desc"
    // buildQuery TagBase builder
    //|> Tags.Load
    Tags.Load("samples/poptags.json")

printfn "Fetch %i most popular tags" popularTags.Items.Length
popularTags.Items
|> Seq.iter (fun t -> printfn "%16s" t.Name)

let userIds =
    popularTags.Items
    |> Seq.collect (fun t ->
        printfn "Fetching most active users for tag \"%s\"" t.Name
        Thread.Sleep(300)
        [
            Users.Load(buildQuery (topUsersByTag Asker All t.Name) noopBuilder).Items
            Users.Load(buildQuery (topUsersByTag Asker Month t.Name) noopBuilder).Items
            Users.Load(buildQuery (topUsersByTag Answerer All t.Name) noopBuilder).Items
            Users.Load(buildQuery (topUsersByTag Answerer Month t.Name) noopBuilder).Items
            //Users.Load("samples/users.json").Items
        ]
        |> Seq.concat
        |> Seq.map (fun u -> u.User.UserId))
    |> Set.ofSeq
    |> List.ofSeq

printfn "Fetched %i users" userIds.Length

let activeTagsByUser =
    userIds
    |> Seq.chunkBySize 100
    |> Seq.collect (fun chunk ->
        Thread.Sleep(60000)
        printfn "Querying another chunk..."
        Tags.Load(buildQuery (tagsByUser chunk) (maxSize 100)).Items
        // Tags.Load("samples/tagswithuser.json").Items
    )

printfn "Completed fetching data. Writing to disk..."
frameData userIds activeTagsByUser popularTags.Items |> writeData "test.csv"