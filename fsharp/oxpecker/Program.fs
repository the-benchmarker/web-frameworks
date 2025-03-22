open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Oxpecker

let emptyHandler: EndpointHandler = fun _ -> Task.CompletedTask

let userIdHandler: EndpointHandler =
    fun ctx ->
        match ctx.TryGetRouteValue<string>("id") with
        | Some id -> ctx.WriteText(id)
        | None -> Task.CompletedTask

let endpoints =
    [ GET [ route "/" emptyHandler; route "/user/{id?}" userIdHandler ]
      POST [ route "/user" emptyHandler ] ]

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
