module Program 

open System.Threading.Tasks
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

module Response =
    let ofBlank : HttpHandler =
        fun _ -> Task.CompletedTask

[<EntryPoint>]
let main args =       
    Host.startWebHost 
        args 
        configureWebHost
        [
            get  "/user/{id}" (Request.mapRoute (fun m -> m.["id"]) Response.ofPlainText)
            post "/user"      (Response.ofBlank)
            get  "/"          (Response.ofBlank)
        ]
    0