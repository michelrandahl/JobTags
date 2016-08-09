module DatabaseFunctions

open FSharp.Data.Sql
open System
open Utils
open JobInformationDefinitions
open JobInformationExtraction

[<Literal>]
let connection_string = "Data Source=jobs.db; Version=3;"

type SQLite = SqlDataProvider< Common.DatabaseProviderTypes.SQLITE, connection_string >
let db = SQLite.GetDataContext()

// a few type aliases for convenience and documentation
type JobEntity = SQLite.dataContext.``main.jobEntity``
type TagEntity = SQLite.dataContext.``main.tagEntity``
type JobTagEntity = SQLite.dataContext.``main.job_tagEntity``
type RareWordsEntity = SQLite.dataContext.``main.rare_wordsEntity``

type JobPosting with
    static member SaveToDB (job_posting : JobPosting) : Either<string,JobEntity option> =
        let meta_info = job_posting.extracted_meta_info
        let basic = job_posting.basic_description
        let existing_job =
            db.Main.Job
            |> Seq.tryFind (fun j -> j.Url = basic.url)
        let create_new_job () =
            try
                let job = db.Main.Job.Create()
                job.Title <- basic.title
                match basic.brief_description with
                | Some brief -> job.BriefDescription <- brief
                | None -> ()
                job.Url <- basic.url
                job.Recruiter <- meta_info.recruiter
                job.Posted <- meta_info.posted.ToString()
                job.Closes <- meta_info.closes.ToString()
                job.ScrapedFrom <- meta_info.scraped_from
                match meta_info.location with
                | Some loc -> job.Location <- loc
                | None -> ()
                match meta_info.industry with
                | Some indu -> job.Industry <- indu
                | None -> ()
                match meta_info.linkedin_views with
                | Some views -> job.LinkedinViews <- views
                | None -> ()
                match meta_info.job_function with
                | Some func -> job.JobFunction <- func
                | None -> ()
                job.LongDescription <- job_posting.long_description
                db.SubmitUpdates()
                Some job
                |> Right
            with e ->
                sprintf "failed to create: %s\n%s" basic.title basic.url
                |> functionFailWith <@ JobPosting.SaveToDB @>
        match existing_job with
        | Some existing_job ->
            if existing_job.Closes <> meta_info.closes.ToString()
            then
                printfn "updating job %s" existing_job.Title
                existing_job.Closes <- meta_info.closes.ToString()
                db.SubmitUpdates()
                Some existing_job |> Right
            else None |> Right
        | None -> create_new_job()

