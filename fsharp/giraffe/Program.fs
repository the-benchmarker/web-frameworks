open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Microsoft.Extensions.Hosting

// ---------------------------------
// Web app
// ---------------------------------

let webApp =
    choose
        [ GET >=> choose
                      [ route "/" >=> text ""
                        routef "/user/%s" text ]
          POST >=> route "/user" >=> text ""
          setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Config and Main
// ---------------------------------

let configureApp (app: IApplicationBuilder) = app.UseGiraffe(webApp)

let configureServices (services: IServiceCollection) = services.AddGiraffe() |> ignore

[<EntryPoint>]
let main args =
    Host.CreateDefaultBuilder(args)
        .ConfigureWebHost(fun webHost ->
            webHost.UseKestrel()
                   .Configure(Action<IApplicationBuilder> configureApp)
                   |> ignore)
        .ConfigureServices(configureServices).Build().Run()
    0
