// Learn more about F# at http://fsharp.org

open System
open Giraffe.ResponseWriters
open Giraffe.Core
open Saturn
open Microsoft.Extensions.Logging

let topRouter = router {
    get "/" (text "")
    getf "/user/%s" text
    post "/user" (text "")

    not_found_handler (setStatusCode 404 >=> text "Not Found")
}

let app = application {
    use_router topRouter
    logging (fun logger -> logger.ClearProviders() |> ignore)
}

run app
