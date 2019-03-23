open Api
open Saturn
open Giraffe
open System.IO
open Microsoft.WindowsAzure.Storage
open Microsoft.Extensions.DependencyInjection

let tryGetEnv =
    System.Environment.GetEnvironmentVariable
    >> function
    | null
    | "" -> None
    | x -> Some x

let publicPath =
    tryGetEnv "public_path"
    |> Option.defaultValue "../Client/public"
    |> Path.GetFullPath

let storageAccount =
    tryGetEnv "STORAGE_CONNECTIONSTRING"
    |> Option.defaultValue "UseDevelopmentStorage=true"
    |> CloudStorageAccount.Parse

let port =
    "SERVER_PORT"
    |> tryGetEnv
    |> Option.map uint16
    |> Option.defaultValue 8085us

let clientPath = Path.Combine("..", "Client") |> Path.GetFullPath
let webApp =
    router { get "/" (htmlFile (Path.Combine(clientPath, "/index.html"))) }

let mainRouter =
    router {
        forward "/api" apiRouter
        forward "" webApp
    }

let configureSerialization (services : IServiceCollection) =
    services.AddSingleton<Giraffe.Serialization.Json.IJsonSerializer>
        (Thoth.Json.Giraffe.ThothSerializer())

let app =
    application {
        url ("http://0.0.0.0:" + port.ToString() + "/")
        use_router mainRouter
        memory_cache
        use_static publicPath
        service_config configureSerialization
        use_gzip
    }

run app
