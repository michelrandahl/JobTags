#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "System.Xml.Linq.dll"
#load "Utils.fs"
#load "JobInformationDefinitions.fs"
#load "JobInformationExtraction.fs"

open FSharp.Data
open System
open Utils
open JobInformationDefinitions
open JobInformationExtraction

let base_url = "https://www.jobfinder.dk"
let url = "https://www.jobfinder.dk/en-gb/jobs/it-and-software-development/"

// example page: https://www.jobfinder.dk/en-gb/jobs/it-and-software-development/
type JobFinderListing = HtmlProvider<"jobfinder_listing_sample.html">

// example page: https://www.jobfinder.dk/en-gb/job/329519738/it-chef/
type JobFinderPost = HtmlProvider<"jobfinder_post_sample.html">

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

/// parse base job information from a div
/// example page: https://www.jobfinder.dk/en-gb/jobs/it-and-software-development/2/
let rec parseBasicJobInfoFromDiv (div : HtmlNode)
                                 : Either<string,BasicJobPostingDescription> =
    try either {
        let! title, uri =
            match div.CssSelect "h3>a"
                  |> Seq.tryHead
                  with
                  | Some a -> Right (a.InnerText(), a.AttributeValue "href")
                  | None -> "no title found"
                            |> functionFailWith <@ parseBasicJobInfoFromDiv @>
        let! description =
            match div.CssSelect "[itemscope='description']"
                  |> Seq.tryHead
                  with
                  | Some description -> Right (description.InnerText())
                  | None -> sprintf "no description for %s" title
                            |> functionFailWith <@ parseBasicJobInfoFromDiv @>
        let url : string = base_url + uri
        return { title = title
                 brief_description = Some description
                 url = url } }
    with e ->
        sprintf "%A" e
        |> functionFailWith <@ parseBasicJobInfoFromDiv @>

/// collect all brief job descriptions from a job listing page
/// example page: https://www.jobfinder.dk/en-gb/jobs/it-and-software-development/2/
let getBriefJobDescriptions (html : JobFinderListing)
                            : Either<string,BasicJobPostingDescription> seq =
    html.Lists.Html.CssSelect "[itemtype='https://schema.org/JobPosting']"
    |> Seq.map parseBasicJobInfoFromDiv

/// get meta info from a job post
/// example page: https://www.jobfinder.dk/en-gb/job/329519738/it-chef/
let extractMetaInfo (html: JobFinderPost) : Either<string,ExtractedMetaInfo> =
    try either {
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
                 location = Some location
                 posted = posted
                 closes = closes
                 linkedin_views = None
                 job_function = None
                 industry = None } }
    with e ->
        sprintf "error in function extractMetaInfo\n%A" e
        |> Left

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
                                 |> Seq.take 5
        let! some_description = brief_descriptions |> Seq.skip 1 |> Seq.head
        printfn "a description"
        printfn "%A\n" some_description

        // get meta info for the job
        let! jobpost_page = some_description.url |> webReader
        let! meta_info = JobFinderPost.Parse jobpost_page
                         |> extractMetaInfo
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

