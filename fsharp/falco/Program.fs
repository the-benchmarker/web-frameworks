module Program 

open Falco
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging

let configureWebHost : Host.ConfigureWebHost =
    let configureLogging (log : ILoggingBuilder) =
        log.ClearProviders()
        |> ignore

    let configureServices (services : IServiceCollection) =
        services.AddRouting() 
        |> ignore

    let configure (routes : HttpEndpoint list) (app : IApplicationBuilder) = 
        app.UseRouting()
            .UseHttpEndPoints(routes)       
            |> ignore 
    
    fun endpoints webHost ->
        webHost
            .UseKestrel()
            .ConfigureLogging(configureLogging)
            .ConfigureServices(configureServices)
            .Configure(configure endpoints)
            |> ignore

module Request =
    let mapRoute
        (map : Map<string, string> -> 'a) 
        (next : 'a -> HttpHandler) : HttpHandler = 
        fun ctx -> next (Request.getRouteValues ctx |> map) ctx

[<EntryPoint>]
let main args =       
    Host.startWebHost 
        args 
        configureWebHost
        [
            get  "/user/{id}" (Request.mapRoute (fun m -> m.["id"]) Response.ofPlainText)
            post "/user"      (Response.ofPlainText "")
            get  "/"          (Response.ofPlainText "")
        ]
    0