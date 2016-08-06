#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "System.Xml.Linq.dll"

open FSharp.Data
open System
open System.IO
open System.Net
open System.Net.Security
open System.Threading
open System.Security.Cryptography.X509Certificates
open System.Text.RegularExpressions


type Either<'a,'b> = Left of 'a
                   | Right of 'b
type EitherBuilder() =
    member this.Bind (x,f) =
        match x with
        | Left msg -> Left msg
        | Right result -> f result
    member this.Return x =
        Right x
let either = new EitherBuilder()

let isRunningMono = Type.GetType("Mono.Runtime") <> null
printfn "running mono: %b" isRunningMono

// ssl in mono is weird, so curl to the rescue!
/// web reader function for unix
let webReaderUnix (url: string) : Either<string, string> =
    try
        let proc = new System.Diagnostics.Process()
        proc.EnableRaisingEvents <- false
        proc.StartInfo.RedirectStandardOutput <- true
        proc.StartInfo.UseShellExecute <- false
        proc.StartInfo.FileName <- "curl"
        proc.StartInfo.Arguments <- (sprintf "-s %s" url)
        proc.Start() |> ignore
        proc.StandardOutput.ReadToEnd()
        |> Right
    with exp -> Left (sprintf "ERROR IN 'webReaderUnix' FOR URL %s\n\n%A" url exp)

/// web reader function for windows
let webReaderNet (url: string) : Either<string,string> =
    try
        // what security!?
        ServicePointManager.ServerCertificateValidationCallback <- (fun _ _ _ _ -> true)
        ServicePointManager.Expect100Continue <- true
        ServicePointManager.SecurityProtocol <- SecurityProtocolType.Tls12
        let wr = WebRequest.CreateHttp url
        wr.UserAgent <- "test.bot" //"safari"
        let stream = wr.GetResponse().GetResponseStream()
        Right( (new StreamReader(stream)).ReadToEnd() )
    with exp -> Left (sprintf "ERROR IN 'web_reader' FOR URL %s\n\n%A" url exp)

let webReader (url: string) : Either<string,string> =
    if isRunningMono
    then webReaderUnix url
    else webReaderNet url

type KeyWord = Single of string
             | Composite of string * string

// make it possible to let the user specify keywords
let language_keywords = [
    Single "matlab"
    Single "c#"
    Single "f#"
    Single "csharp"
    Single "fsharp"
    Single "c++"
    Single "java"
    Single "java"
    Single "python"
    Single ".net"
    Single "visual-basic"
    Single "vb.net"
    Single "lua"
    Single "haskell"
    Single "idris"
    Single "erlang"
    Single "vhdl"
    Single "elixir"
    Single "javascript"
    Single "nodejs"
    Single "node"
    Single "html5"
    Single "jvm"
    Single "scala"
    Single "clojure"
    Single "apl"
    Single "cuda"
    Single "r"
    Single "c"
    Single "vb"
    Single "ruby"
    Single "sql"
    ]

// make it possible to let the user specify keywords
let other_keywords = [
    Single "firmware"
    Single "programmering"
    Single "rtos"
    Single "crm"
    Single "linux"
    Single "windows"
    Composite ("machine","learning")
    Composite ("design","patterns")
    ]

let base_url = "https://www.jobfinder.dk"
let url = "https://www.jobfinder.dk/en-gb/jobs/it-and-software-development/"

// example page: https://www.jobfinder.dk/en-gb/jobs/it-and-software-development/
type JobFinderListing = HtmlProvider<"jobfinder_listing_sample.html">

// example page: https://www.jobfinder.dk/en-gb/job/329519738/it-chef/
type JobFinderPost = HtmlProvider<"jobfinder_post_sample.html">

type JobPostingDescription = {
    title : string
    description : string
    url : string
    }

type MetaInfo = {
    recruiter : string
    location : string
    posted : DateTime
    closes : DateTime
    }

type JobPosting = {
    description : JobPostingDescription
    long_description : string
    language_tags : string list
    meta_info : MetaInfo
    }

/// fetch link for next job listing url from next button in the bottom of the page
/// example page: https://www.jobfinder.dk/en-gb/jobs/it-and-software-development/
let getNextJobListingUrl (html : JobFinderListing) : string option =
    let content = html.Lists.Html.ToString()
    html.Lists.Html.CssSelect "[rel='next']"
    |> Seq.tryHead
    |> Option.map (fun x -> base_url + x.AttributeValue "href")

/// collect all job listing urls from next buttons
/// example page: https://www.jobfinder.dk/en-gb/jobs/it-and-software-development/
let collectJobListingUrls (url : string) : Either<string,string list> =
    let rec loop (url : string option) = seq {
        match url with
        | Some url ->
            yield Right url
            match webReader url with
            | Right html ->
                yield! html
                       |> JobFinderListing.Parse
                       |> getNextJobListingUrl
                       |> loop
            | Left err -> yield Left err
        | None -> () }

    loop (Some url)
    |> Seq.fold (fun xs url -> either {
                 let! xs = xs
                 let! url = url
                 return url :: xs })
                (Right [])

/// parse brief job description from a div
/// example page: https://www.jobfinder.dk/en-gb/jobs/it-and-software-development/2/
let parseBriefDescriptionFromDiv (div : HtmlNode)
                                 : Either<string,JobPostingDescription>
                                 = either {
    let! title, uri =
        match div.CssSelect "h3>a"
              |> Seq.tryHead
              with
              | Some a -> Right (a.InnerText(), a.AttributeValue "href")
              | None -> Left "no title found"
    let! description =
        match div.CssSelect "[itemscope='description']"
              |> Seq.tryHead
              with
              | Some description -> Right (description.InnerText())
              | None -> Left (sprintf "no description for %s" title)
    let url : string = base_url + uri
    return { title = title; description = description; url = url } }

/// collect all brief job descriptions from a job listing page
/// example page: https://www.jobfinder.dk/en-gb/jobs/it-and-software-development/2/
let getBriefJobDescriptions (html : JobFinderListing)
                       : Either<string,JobPostingDescription> seq =
    html.Lists.Html.CssSelect "[itemtype='https://schema.org/JobPosting']"
    |> Seq.map parseBriefDescriptionFromDiv

/// get meta info from a job post
/// example page: https://www.jobfinder.dk/en-gb/job/329519738/it-chef/
let getMetaInfo (html: JobFinderPost) : Either<string,MetaInfo> = either {
    let! div_block =
        match html.Lists.Html.CssSelect "div"
              |> Seq.filter
                  (fun div ->
                   match div.CssSelect "[itemprop='hiringOrganization']" with
                   | [] -> false
                   | _ -> true)
              |> Seq.tryHead
              with
              | Some div -> Right div
              | None -> Left "no div with [itemprop='hiringOrganization']"
    let getRow (n : int) f =
        match div_block.CssSelect "dd"
              |> Seq.skip n
              |> Seq.tryHead
              with
              | Some row -> f(row.InnerText()) |> Right
              | None -> sprintf "failed to extract row number %i" n |> Left
    let! recruiter = getRow 0 id
    let! location  = getRow 1 id
    let! posted    = getRow 2 (DateTime.Parse)
    let! closes    = getRow 3 (DateTime.Parse)
    return { recruiter = recruiter
             location = location
             posted = posted
             closes = closes } }

let getLongDescription (html: JobFinderPost) : Either<string,string> =
    match html.Lists.Html.CssSelect "[itemprop='description']"
          |> Seq.tryHead
          |> Option.map (fun html -> html.Elements())
          |> Option.map
              (Seq.map (fun html ->
                        match html.Name() with
                        | "p"  -> html.InnerText()
                        | "ul" -> html.CssSelect "li"
                                  |> Seq.map (fun li -> " - " + li.InnerText())
                                  |> String.concat "\n"
                        | _ -> "\n"))
          |> Option.map (String.concat "\n\n")
          with
          | Some result -> Right result
          | None -> Left "getLongDescription failed"

let getDescriptionWords (description : string) : string list =
    let description_words =
        description.Split ' '
        |> Seq.map (fun x -> x.Trim())
        |> Seq.collect (fun x -> x.Split '/') // .NET/C#
        |> Seq.choose (fun x ->
                       let m = Regex("[\w#\+\.\-]+").Match x
                       if m.Success
                       then Some m.Value
                       else None)
        |> Seq.map (fun x -> x.TrimEnd '.')
        |> Seq.map (fun x -> x.TrimEnd '-')
        |> Seq.map (fun x -> x.TrimEnd '+')
        |> Seq.filter ((<>)"")
        |> Seq.filter (fun x -> // remove numbers
            match Int32.TryParse x with
            | true, _ -> false
            | _ -> true)
        |> Seq.map (fun x -> x.ToLower())
    description_words
    |> List.ofSeq

let getTags (keywords : KeyWord list) (description_words : string list) : string list =
    let non_composite_keywords =
        keywords
        |> Seq.choose (fun k ->
                      match k with
                      | Single k -> Some k
                      | _ -> None)
        |> Set.ofSeq
    let composite_keywords =
        keywords
        |> Seq.choose (fun k ->
                       match k with
                       | Composite(k1,k2) -> Some (k1,k2)
                       | _ -> None)
        |> Set.ofSeq
    let non_composite_tags =
        description_words
        |> Set.ofList
        |> Seq.filter (fun x -> Set.contains x non_composite_keywords)
        |> List.ofSeq
    let composite_tags =
        description_words
        |> Seq.windowed 2
        |> Seq.map (fun [|x;y|] -> x,y)
        |> Seq.filter (fun x -> Set.contains x composite_keywords)
        |> Seq.map (fun (x,y) -> sprintf "%s %s" x y)
        |> List.ofSeq
    List.concat [non_composite_tags; composite_tags]

let countWords (words : string seq) =
    words
    |> Seq.countBy id

let getRareWords (n : int) (word_count : Map<string,int>) (words : string seq) =
    words
    |> Seq.sortBy (fun w ->
                   match Map.tryFind w word_count with
                   | Some count -> count
                   | None -> 1)
    |> Seq.take n


// small manual test
let res =
    either {
        // collect all joblisting urls
        let! urls = collectJobListingUrls url
        printfn "job listings urls"
        List.ofSeq urls
        |> printfn "%A\n"

        // get the first brief job description available
        let! some_page = List.head urls |> webReader
        let brief_descriptions = JobFinderListing.Parse some_page
                                 |> getBriefJobDescriptions
                                 |> Seq.take 7
        let! some_description = brief_descriptions |> Seq.skip 1 |> Seq.head
        printfn "a description"
        printfn "%A\n" some_description

        // get meta info for the job
        let! jobpost_page = some_description.url |> webReader
        let! meta_info = JobFinderPost.Parse jobpost_page
                         |> getMetaInfo
        printfn "some meta info"
        printfn "%A\n" meta_info

        // get long description for the job
        let! long_description =
            JobFinderPost.Parse jobpost_page
            |> getLongDescription
        printfn "long job description"
        printfn "%s\n" long_description

        // get tags
        let description_words = getDescriptionWords long_description
        printfn "description words"
        printfn "%A\n" description_words
        let lang_tags = getTags language_keywords description_words
        printfn "extracted language keywords"
        printfn "%A" lang_tags
        let other_tags = getTags other_keywords description_words
        printfn "extracted other keywords"
        printfn "%A\n" other_tags

        // get word count to calculate rare words
        let! all_words =
            brief_descriptions
            |> Seq.map (fun d -> either {
                        let! desc = d
                        let url = desc.url
                        let! jobpost = webReader url
                        let! long_desc =
                            JobFinderPost.Parse jobpost
                            |> getLongDescription
                        let words = getDescriptionWords long_desc
                        return words })
            |> Seq.fold (fun xs x -> either {
                         let! xs' = xs
                         let! x' = x
                         return x' :: xs' })
                        (Right [])
        let word_count =
            List.concat all_words
            |> countWords
            |> Map.ofSeq
        // get 5 rare words for the previously extracted description
        printfn "word count"
        printfn "%A" word_count
        let rare_words =
            getRareWords 5 word_count description_words
            |> List.ofSeq
        printfn "rare words"
        printfn "%A\n" rare_words

        return "it all seems to work!"
        }
printfn "%A" res

