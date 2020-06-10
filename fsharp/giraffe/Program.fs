open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Giraffe

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
let main _ =
    WebHostBuilder().UseKestrel().Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices).Build().Run()
    0
