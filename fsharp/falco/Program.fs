open Falco

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

module Server =
    module Config =         
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

    module Handlers = 
        let defaultMsg = ""

        let handleDefault : HttpHandler =  
            textOut defaultMsg

        let handleUserId : HttpHandler =
            fun next ctx -> 
                let userId = ctx.TryGetRouteValue "id" |> Option.defaultValue ""
                textOut userId next ctx

    let buildServer (webHostBuilder : IWebHostBuilder) =        
        let routes = 
            [
                get  "/user/{id}" Handlers.handleUserId
                post "/user"      Handlers.handleDefault
                get  "/"          Handlers.handleDefault
            ]

        webHostBuilder     
            .UseKestrel()
            .ConfigureLogging(Config.configureLogging)
            .ConfigureServices(Config.configureServices)
            .Configure(Config.configure routes)                   
            |> ignore

[<EntryPoint>]
let main args =    
    Host.CreateDefaultBuilder(args)
        .ConfigureWebHost(fun webHost -> Server.buildServer webHost)
        .Build()
        .Run()
    0