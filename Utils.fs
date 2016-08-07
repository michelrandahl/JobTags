module Utils

open System
open System.Net.Security
open System.Net
open System.Threading
open System.Security.Cryptography.X509Certificates
open System.IO
open Microsoft.FSharp.Quotations


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


/// experimental reflection to get function name for error logging
/// use like this getFunctionName <@ someFunction @>
/// unfortunatly it can only be used inside recursive functions
let rec getFunctionName =
    function
    | Patterns.Call(None, methodInfo, _) -> Right methodInfo.Name
    | Patterns.Lambda(_, expr) -> getFunctionName expr
    | _ -> Left "error in Utils.getFunctionName"

let functionFailWith (expr : Expr) (msg : string) =
    match getFunctionName expr with
    | Right name ->
        sprintf "ERROR IN FUNCTION %s\n%s" name msg
        |> Left
    | Left err -> Left err


let isRunningMono = Type.GetType("Mono.Runtime") <> null
printfn "you are running mono: %b!" isRunningMono

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

/// web reader function for .net
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
