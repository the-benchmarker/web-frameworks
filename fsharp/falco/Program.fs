open Falco
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

let noContentHandler =
    fun next (ctx: HttpContext) ->
        setStatusCode 200 next ctx

let userIdHandler =
    fun next (ctx: HttpContext) ->
        let userId = ctx.TryGetRouteValue "id" |> Option.defaultValue ""
        textOut userId next ctx

let routes =
    [
        get "/" noContentHandler
        get "/user/{id}" userIdHandler
        get "/user" noContentHandler
    ]

// Enable services (routing required for Falco)
let configureServices (services : IServiceCollection) =
    services.AddRouting() |> ignore

// Activate middleware
let configureApp (app : IApplicationBuilder) = 
    app.UseRouting().UseHttpEndPoints(routes)
        .UseNotFoundHandler(setStatusCode 404 >=> textOut "Not found") |> ignore

[<EntryPoint>]
let main _ =
    WebHostBuilder().UseKestrel().ConfigureServices(configureServices)
        .Configure(configureApp).Build().Run()
    0
