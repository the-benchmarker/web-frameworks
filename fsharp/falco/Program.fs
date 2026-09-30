module Program

open Falco
open Falco.Routing
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Logging

[<EntryPoint>]
let main args =
    let bldr = WebApplication.CreateBuilder(args)
    bldr.Logging.ClearProviders() |> ignore

    let wapp = bldr.Build()

    wapp.UseRouting()
        .UseFalco([
            mapGet  "/user/{id}" (fun r -> r.GetString "id") Response.ofPlainText
            post "/user" Response.ofEmpty
            get  "/" Response.ofEmpty
        ])
        .Run()
    0
