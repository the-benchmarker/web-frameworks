open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Giraffe

// ---------------------------------
// Web app
// ---------------------------------

let webApp: HttpFunc -> HttpFunc =
    choose [ routef "/user/%s" (fun name -> GET >=> text name)
             route "/user" >=> POST >=> text ""
             route "/" >=> GET >=> text ""
             setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Config and Main
// ---------------------------------

let configureApp (app: IApplicationBuilder) = app.UseGiraffe(webApp)

let configureServices (services: IServiceCollection) = services.AddGiraffe() |> ignore

let args = System.Environment.GetCommandLineArgs()

Host.CreateDefaultBuilder(args)
    .ConfigureWebHost(fun webHost ->
        webHost.UseKestrel()
                .ConfigureServices(configureServices)
                .Configure(Action<IApplicationBuilder> configureApp)
                |> ignore)
    .Build().Run()
