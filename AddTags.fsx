#r "../../packages/SQLProvider/lib/FSharp.Data.SqlProvider.dll"
#r "../../packages/SQLProvider/lib/Mono.Data.Sqlite.dll"

#load "Utils.fs"
#load "JobInformationDefinitions.fs"
#load "JobInformationExtraction.fs"
#load "DatabaseFunctions.fs"

open System
open System.Threading
open JobInformationDefinitions
open JobInformationExtraction
open DatabaseFunctions

let jobs : JobEntity list = db.Main.Job |> List.ofSeq

let jobs_and_words : (JobEntity * string list) list =
    jobs |> List.map (fun j -> j, getDescriptionWords j.LongDescription)

let word_count : Map<string,int> =
    jobs_and_words
    |> Seq.map snd
    |> Seq.concat
    |> countWords

let jobs_and_tags_and_rare_words : (JobEntity * string list * string list) list =
    jobs_and_words
    |> List.map (fun (job,words) ->
                 let lang_tags = getTags language_keywords words
                 let other_tags = getTags other_keywords words
                 job,
                 List.concat [lang_tags; other_tags],
                 getRareWords 5 word_count words)

jobs_and_tags_and_rare_words
|> Seq.iter (fun (job, tags,rare_words) ->
             (job.Title, tags, rare_words)
             |||> printfn "%s - tags: %A - rare: %A")

jobs |> Seq.length
|> printfn "total jobs %i"

word_count |> Map.toList |> List.length
|> printfn "total words %i"

