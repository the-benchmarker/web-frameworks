open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
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

let configureServices (services: IServiceCollection) = services.AddRouting() |> ignore

[<EntryPoint>]
let main args =
    Host
        .CreateDefaultBuilder(args)
        .ConfigureWebHost(fun webHost ->
            webHost
                .UseKestrel()
                .ConfigureServices(configureServices)
                .Configure(configureApp)
            |> ignore)
        .Build()
        .Run()

    0
