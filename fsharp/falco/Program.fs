open Falco
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection

let defaultHandler : HttpHandler =  
    (textOut "")

let userIdHandler : HttpHandler =
    fun next ctx -> textOut (ctx.TryGetRouteValue "id" |> Option.defaultValue "") next ctx

let routes = 
    [
        get  "/user/{id}" userIdHandler
        post "/user"      defaultHandler
        get  "/"          defaultHandler
    ]

let configureLogging (log : ILoggingBuilder) =
    log.ClearProviders()
    |> ignore

let configureServices (services : IServiceCollection) =
    services.AddRouting() 
    |> ignore

let configure (app : IApplicationBuilder) = 
    app.UseRouting()
       .UseHttpEndPoints(routes)       
       |> ignore 

[<EntryPoint>]
let main _ =
    try
        WebHostBuilder()
            .UseKestrel()
            .ConfigureLogging(configureLogging)
            .ConfigureServices(configureServices)
            .Configure(configure)
            .Build()
            .Run()
        0
    with 
        | _ -> -1