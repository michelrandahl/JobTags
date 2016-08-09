#r "../../packages/SQLProvider/lib/FSharp.Data.SqlProvider.dll"
#r "../../packages/SQLProvider/lib/Mono.Data.Sqlite.dll"
#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "System.Xml.Linq.dll"
#load "Utils.fs"
#load "JobInformationDefinitions.fs"
#load "JobInformationExtraction.fs"
#load "DatabaseFunctions.fs"

open FSharp.Data
open FSharp.Data.Sql
open System
open Utils
open JobInformationDefinitions
open JobInformationExtraction
open DatabaseFunctions

type LinkedInJobs = JsonProvider<"samples/linkedin_dump.json">

let jobs =
    LinkedInJobs.Load "linkedin_dump.json"
    |> List.ofSeq
    |> List.choose (fun job -> maybe {
                    let! title = job.Title
                    let! description = job.Description
                    let url = job.Url
                    let! closes = job.Till
                    let! posted = job.From
                    let! recruiter = job.Company.Name
                    let basic = {
                        title = title
                        brief_description = None
                        url = url }
                    let meta_info = {
                        recruiter = recruiter
                        posted = posted
                        closes = closes
                        scraped_from = "LinkedIn"
                        location = None
                        industry = job.Industry
                        linkedin_views = job.Views
                        job_function = job.Jobfunction }
                    let posting : JobPosting = {
                       basic_description = basic
                       long_description = description
                       extracted_meta_info = meta_info
                       derived_meta_info = None }
                    return posting })
    |> List.map (JobPosting.SaveToDB)
    |> List.fold (fun xs x -> either {
                  let! x = x
                  let! xs = xs
                  return x :: xs})
                 (Right [])

either {
    let! jobs = jobs
    jobs
    |> List.choose id
    |> List.length
    |> printfn "saved jobs %i"
    return ()
    }
|> printfn "%A"


