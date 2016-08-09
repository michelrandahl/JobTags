(*
   scrape JobFinder for jobs and save to a sqlite DB
*)
#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "System.Xml.Linq.dll"
#r "../../packages/SQLProvider/lib/FSharp.Data.SqlProvider.dll"
#r "../../packages/SQLProvider/lib/Mono.Data.Sqlite.dll"
#load "Utils.fs"
#load "JobInformationDefinitions.fs"
#load "JobInformationExtraction.fs"
#load "DatabaseFunctions.fs"

open FSharp.Data
open System
open Utils
open JobInformationDefinitions
open JobInformationExtraction
open DatabaseFunctions

let base_url = "https://www.jobfinder.dk"
let categories_url = "https://www.jobfinder.dk/en-gb/moreterms/jobfunction/101/"

// example page: https://www.jobfinder.dk/en-gb/jobs/it-and-software-development/
type JobFinderListing = HtmlProvider<"samples/jobfinder_listing_sample.html">

// example page: https://www.jobfinder.dk/en-gb/job/329519738/it-chef/
type JobFinderPost = HtmlProvider<"samples/jobfinder_post_sample.html">

// example page: https://www.jobfinder.dk/en-gb/moreterms/jobfunction/101/
type JobFinderCategories = HtmlProvider< "samples/jobfinder_browse_by_jobfunction_sample.html" >

/// collect urls for the various categories
/// example page: https://www.jobfinder.dk/en-gb/moreterms/jobfunction/101/
let rec getCategoryUrls (url : string) =
    try either {
        let! html = webReader url
        let parsed = JobFinderCategories.Parse html
        return parsed.Lists.List6.Html.CssSelect "a"
               |> Seq.map (fun a -> base_url + a.AttributeValue "href")
        }
    with e ->
        (sprintf "%A" e)
        |> functionFailWith <@ getCategoryUrls @>

/// fetch link for next job listing url from next button in the bottom of the page
/// example page: https://www.jobfinder.dk/en-gb/jobs/it-and-software-development/
let getNextJobListingUrl (html : JobFinderListing) : string option =
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
let getBasicJobDescriptions (html : JobFinderListing)
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
                 scraped_from = "JobFinder"
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

let scrapeJobs (url : string) = either {
    let! category_links = getCategoryUrls url
    let! job_listings_cats_and_urls =
        category_links
        |> Seq.map (fun url -> either {
                    let! job_listings_urls = collectJobListingUrls url
                    return job_listings_urls })
        |> Seq.fold (fun state x -> either {
                     let! x = x
                     let! xs = state
                     return List.concat [x;  xs] })
                    (Right [])
    let! job_listings =
        job_listings_cats_and_urls
        |> Seq.fold (fun state url -> either {
                     let! html = webReader url
                     let! xs = state
                     let parsed = JobFinderListing.Parse html
                     return parsed :: xs })
                    (Right [])
    let! basic_job_postings =
        job_listings
        |> Seq.map getBasicJobDescriptions
        |> Seq.concat
        |> Seq.fold (fun state basic -> either {
                     let! basic = basic
                     let! xs = state
                     return basic :: xs })
                    (Right [])
    let! job_postings =
        basic_job_postings
        |> Seq.map (fun basic -> either {
                    let! html = webReader basic.url
                    let post = JobFinderPost.Parse html
                    let! long_description = getLongDescription post
                    let! meta_info = extractMetaInfo post
                    return { basic_description = basic
                             long_description = long_description
                             extracted_meta_info = meta_info
                             derived_meta_info = None } })
        |> Seq.fold (fun state job_post -> either {
                     let! job_post = job_post
                     let! xs = state
                     return job_post :: xs } )
                    (Right [])
    let total_job_postings =
        job_postings
        |> List.length
    let unique_jobs =
        job_postings
        |> Seq.map (fun j ->
                    (j.basic_description.title,
                     j.extracted_meta_info.recruiter,
                     j.extracted_meta_info.posted,
                     j.extracted_meta_info.closes,
                     j.basic_description.url), j)
        |> Map.ofSeq
        |> Map.toSeq
        |> Seq.map snd
    let! saved_jobs =
        unique_jobs
        |> Seq.fold (fun state job -> either {
                     let! saved_job = JobPosting.SaveToDB job
                     let! xs = state
                     return saved_job :: xs })
                    (Right [])
    let saved_jobs_count = saved_jobs |> List.choose id |> List.length
    let unique_jobs_count = unique_jobs |> Seq.length

    (total_job_postings, unique_jobs_count, saved_jobs_count)
    |||> printfn "total jobs: %i\ntotal unique %i\ntotal saved jobs %i"

    return total_job_postings, unique_jobs_count, saved_jobs_count
    }
scrapeJobs categories_url
|> printfn "%A"
