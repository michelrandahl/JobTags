module JobInformationDefinitions

open System

type BasicJobPostingDescription = {
    title : string
    brief_description : string option
    url : string
    }

type ExtractedMetaInfo = {
    recruiter : string
    posted : DateTime
    closes : DateTime
    location : string option
    industry : string option
    linkedin_views : int option
    job_function : string option
    }

type DerivedMetaInfo = {
    category1_tags : string list
    category2_tags : string list
    rare_words : string list
    }

type JobPosting = {
    basic_description : BasicJobPostingDescription
    long_description : string
    extracted_meta_info : ExtractedMetaInfo
    derived_meta_info : DerivedMetaInfo
    }

type KeyWord = Single of string
             | Composite of string * string

// make it possible to let the user specify keywords
let language_keywords = [
    Single "objective-c"
    Single "swift"
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
    Single "r"
    Single "c"
    Single "vb"
    Single "ruby"
    Single "sql"
    ]

// make it possible to let the user specify keywords
let other_keywords = [
    Single "cuda"
    Single "firmware"
    Single "programmering"
    Single "rtos"
    Single "crm"
    Single "linux"
    Single "windows"
    Composite ("machine","learning")
    Composite ("design","patterns")
    Composite ("unit","test")
    ]
