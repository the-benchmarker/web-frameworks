open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Oxpecker

let userNameHandler (name: string) : EndpointHandler = _.WriteText(name)

let endpoints =
    [ GET [ route "/" <| text ""; routef "/user/{%s}" text ]
      POST [ route "/user" <| text "" ] ]

let configureLogging (log: ILoggingBuilder) = log.ClearProviders() |> ignore

let configureServices (services: IServiceCollection) =
    services.AddRouting().AddOxpecker() |> ignore

let configureApp (app: IApplicationBuilder) =
    app.UseRouting().UseOxpecker(endpoints) |> ignore

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
