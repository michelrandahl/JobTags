module JobInformationDefinitions

open System

(*
  SQLite tables:

  create table job(
    title text not null,
    recruiter text not null,
    posted text not null,
    closes text not null,
    url text not null,
    brief_description text,
    scraped_from text not null,
    location text,
    industry text,
    linkedin_views int,
    job_function text,
    long_description text not null,
    unique(title,recruiter,posted,closes, url)
    );

  create table tag(
    tag_text text not null,
    unique(tag_text)
  );

  create table job_tag(
    job integer,
    tag integer,
    foreign key(job) references job(rowid),
    foreign key(tag) references tag(rowid)
  );

  create table rare_words(
    job integer,
    rare_words text not null,
    foreign key(job) references job(rowid)
    );
*)

type BasicJobPostingDescription = {
    title : string
    brief_description : string option
    url : string
    }

type ExtractedMetaInfo = {
    recruiter : string // company name
    posted : DateTime
    closes : DateTime // deadline for job applications
    scraped_from : string // site from which the data was extracted
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
    derived_meta_info : DerivedMetaInfo option
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
    Single "tcl"
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
    Single "sql" ]

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
    Composite ("unit","test") ]
