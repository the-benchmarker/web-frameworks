module Program 

open Falco
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging

let configureWebHost 
    (routes : HttpEndpoint list)
    (webHost : IWebHostBuilder) =
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

    webHost
        .UseKestrel()
        .ConfigureLogging(configureLogging)
        .ConfigureServices(configureServices)
        .Configure(configure routes)
        |> ignore

[<EntryPoint>]
let main args =   
    let message = ""

    Host.startWebHost 
        args 
        configureWebHost
        [
            get  "/user/{id}" (fun ctx -> Response.ofPlainText (Request.tryGetRouteValue "id" ctx |> Option.defaultValue "") ctx)
            post "/user"      (Response.ofPlainText message)
            get  "/"          (Response.ofPlainText message)
        ]
    0