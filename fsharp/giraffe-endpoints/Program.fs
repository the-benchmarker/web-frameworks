open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Giraffe
open Giraffe.EndpointRouting

// ---------------------------------
// Web app
// ---------------------------------

let webApp =
    [ GET [ routef "/user/%s" text
            route "/" (text "") ]
      POST [ route "/user" (text "") ] ]

// ---------------------------------
// Config and Main
// ---------------------------------

let configureApp (app: IApplicationBuilder) =
    app
        .UseRouting()
        .UseEndpoints(fun e -> e.MapGiraffeEndpoints(webApp))
    |> ignore

let configureLogging (log : ILoggingBuilder) =
    log.ClearProviders()
    |> ignore

let configureServices (services: IServiceCollection) = services.AddRouting() |> ignore

let args = System.Environment.GetCommandLineArgs()

Host
    .CreateDefaultBuilder(args)
    .ConfigureWebHost(fun webHost ->
        webHost
            .UseKestrel(fun c -> c.AddServerHeader <- false)
            .ConfigureLogging(configureLogging)
            .ConfigureServices(configureServices)
            .Configure(configureApp)
        |> ignore)
    .Build()
    .Run()
