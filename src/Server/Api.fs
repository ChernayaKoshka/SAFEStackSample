module Api

open Giraffe
open Saturn
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2
open System.Collections.Concurrent
open Shared
open System.IO
open System.Net
open FSharp.Data

type Microsoft.AspNetCore.Http.HttpContext with
    member this.RemoteIpAddress =
        this.Request.Headers.["X-Forwarded-RemoteAddress"]
        |> Seq.tryHead
        |> Option.map (fun ip -> IPAddress.Parse(ip))
        |> Option.defaultValue (this.Connection.RemoteIpAddress)

let comments = ConcurrentBag<Comment * IPAddress>()

type CommentCsv = CsvProvider< @".\Samples\CommentCsvSample.csv" >

let commentFilePath =
    Path.Combine(Directory.GetCurrentDirectory(), @"comments.csv")

if not (File.Exists(commentFilePath)) then
    use file = File.AppendText("comments.csv")
    file.WriteLine("Author,Comment,Mood,IPAddress")
    file.Flush()
    file.Dispose()

CommentCsv.Load(commentFilePath).Rows
|> Seq.iter
       (fun item ->
       comments.Add
           ({ Author = item.Author
              CommentText = item.Comment
              Mood = Some(Mood.FromStr item.Mood) },
            IPAddress.Parse(item.IPAddress)))

let invalidRequest errMsg next (ctx : HttpContext) =
    ctx.SetStatusCode 400
    text errMsg next ctx

let tooManyRequests errMsg next (ctx : HttpContext) =
    ctx.SetStatusCode 429
    text errMsg next ctx

let addComment next (ctx : HttpContext) =
    task {
        let! toAdd = ctx.BindModelAsync<Comment>()
        let ip = ctx.RemoteIpAddress
        printfn "Incoming comment from %s. Comment: %A" (ip.ToString()) toAdd
        match Validation.validateComment toAdd with
        | Ok comment ->
            match comments |> Seq.tryFind (snd >> (=) ip) with
            | Some(comment, _) ->
                return! tooManyRequests
                            (sprintf "%s, you've already commented."
                                 comment.Author) next ctx
            | None ->
                comments.Add(comment, ip)
                // Should probably have DB logic here, but this was the easy way out for a sample app
                File.AppendAllLines
                    (commentFilePath,
                     [ sprintf "\"%s\",\"%s\",\"%s\",\"%s\"" comment.Author
                           comment.CommentText (comment.Mood.Value.ToString())
                           (ip.ToString()) ])
                return! text "Added comment!" next ctx
        | Error err -> return! invalidRequest err next ctx
    }

let apiRouter =
    router {
        pipe_through (pipeline { set_header "X-Pipeline-Type" "API" })
        post "/comment" addComment
        get "/comments"
            (fun next ctx ->
            task { return! json (comments.ToArray() |> Array.map fst) next ctx })
    }
