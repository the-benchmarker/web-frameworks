open Falco
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

let defaultHandler = 
    (textOut "")

let userIdHandler =
    fun next (ctx: HttpContext) ->
        let userId = ctx.TryGetRouteValue "id" |> Option.defaultValue ""
        textOut userId next ctx

let routes = 
    [
        get  "/"          defaultHandler
        get  "/user/{id}" userIdHandler
        post "/user"      defaultHandler
    ]

let configureServices (services : IServiceCollection) =
    services.AddRouting() 
    |> ignore

let configureApp (app : IApplicationBuilder) = 
    app.UseRouting()
       .UseHttpEndPoints(routes)       
       |> ignore 

[<EntryPoint>]
let main _ =
    try
        WebHostBuilder()
            .UseKestrel()
            .ConfigureServices(configureServices)
            .Configure(configureApp)
            .Build()
            .Run()
        0
    with 
        | _ -> -1