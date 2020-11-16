module App

open Falco
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging

let run endpoints =
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
        
    let configureWebHost : Host.ConfigureWebHost =
        fun endpoints webHost ->
            webHost
                .UseKestrel(fun c -> c.AddServerHeader <- false)
                .ConfigureLogging(configureLogging)
                .ConfigureServices(configureServices)
                .Configure(configure endpoints)
                |> ignore

    Host.startWebHost [||] configureWebHost endpoints