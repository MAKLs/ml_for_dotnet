#load "references.fsx"
#load "NaiveBayes.fs"
open System.IO
open System.Text.RegularExpressions
open NaiveBayes.Classifier

type DocType =
    | Ham
    | Spam

let dataPath = __SOURCE_DIRECTORY__ + "/../data/SMSSpamCollection"

let identify (sample:DocType*string) =
    let docType,content = sample
    match docType with
    | Ham -> printfn "'%s' is ham" content
    | Spam -> printfn "'%s' is spam" content

let parseType typeStr =
    match typeStr with
    | "ham" -> Ham
    | "spam" -> Spam
    | _ -> failwith typeStr

let parseLine (line:string) =
    let comp = line.Split('\t')
    let label = comp.[0] |> parseType
    (label,comp.[1])

let dataSet = 
    File.ReadAllLines dataPath
    |> Array.map parseLine

// Feature exploration
let top n (tokenizer:Tokenizer) (docs:string[]) =
    let tokenized = docs |> Array.map tokenizer
    let tokens = tokenized |> Set.unionMany
    tokens
    |> Seq.sortBy (fun t -> - countIn tokenized t)
    |> Seq.take n
    |> Set.ofSeq

let bottom n (tokenizer:Tokenizer) (docs:string[]) =
    let tokenized = docs |> Array.map tokenizer
    let tokens = tokenized |> Set.unionMany
    tokens
    |> Seq.sortBy (fun t -> countIn tokenized t)
    |> Seq.take n
    |> Set.ofSeq

// Tokenizers
let matchWords = Regex(@"\w+")
let phoneWords = Regex(@"0[7-9]\d{9}")
let txtCode = Regex(@"\b\d{5}\b")

let wordTokenizer (text:string) =
    text.ToLowerInvariant()
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

let casedTokenizer (text:string) =
    text
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

let phoneTokenizer(text:string) =
    match (phoneWords.IsMatch text) with
    | true -> "__PHONE__"
    | false -> text

let txtTokenizer(text:string) =
    match (txtCode.IsMatch text) with
    | true -> "__TXT__"
    | false -> text

let smartTokenizer = casedTokenizer >> Set.map phoneTokenizer >> Set.map txtTokenizer

// Classifier experiments
let (training,validation) = dataSet.[..1000],dataSet.[1001..]

let vocabulary (tokenizer:Tokenizer) (corpus:string seq) =
    corpus
    |> Seq.map tokenizer
    |> Set.unionMany

let allTokens =
    training
    |> Seq.map snd
    |> vocabulary wordTokenizer

let txtToken = set ["txt"]

let ham,spam =
    let rawHam,rawSpam =
        training
        |> Array.partition (fun (label,_) -> label = Ham)
    rawHam |> Array.map snd,
    rawSpam |> Array.map snd
let hamCount = ham |> vocabulary casedTokenizer |> Set.count
let spamCount = spam |> vocabulary casedTokenizer |> Set.count

let topHam = ham |> top (hamCount / 10) casedTokenizer
let topSpam = spam |> top (spamCount / 25) casedTokenizer
let topTokens = Set.union topHam topSpam
let commonTokens = Set.intersect topHam topSpam
let specificTokens = Set.difference topTokens commonTokens
let smartTokens =
    specificTokens
    |> Set.add "__PHONE__"
    |> Set.add "__TXT__"

// Validation
let evaluate (tokenizer:Tokenizer) (tokens:Token Set) =                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
    let classifier = train training tokenizer tokens
    validation
    |> Seq.averageBy (fun (docType,sms) -> if docType = classifier sms then 1.0 else 0.0)
    |> printfn "Correctly classified %.3f"

let bestModel = train training smartTokenizer smartTokens

validation
|> Seq.filter (fun (docType,_) -> docType = Ham)
|> Seq.averageBy (fun (docType,sms) -> if docType = bestModel sms then 1.0 else 0.0)
|> printfn "Correctly classifed %.3f HAM"
validation
|> Seq.filter (fun (docType,_) -> docType = Spam)
|> Seq.averageBy (fun (docType,sms) -> if docType = bestModel sms then 1.0 else 0.0)
|> printfn "Correctly classifed %.3f SPAM"