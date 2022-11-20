open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open WebSharper
open WebSharper.AspNetCore
open WebSharper.Sitelets

type EndPoint =
    | [<EndPoint "GET /">] Home
    | [<EndPoint "GET /user">] GetUser of id: string
    | [<EndPoint "POST /user">] User

module Site =
    let encoding = System.Text.UTF8Encoding(false)
    let Main =
        Application.MultiPage(fun _ endpoint ->
            match endpoint with
            | Home -> Content.Ok
            | GetUser id -> Content.Text(id, encoding=encoding)
            | User -> Content.Ok)

type Website(config: IConfiguration) =
    inherit SiteletService<EndPoint>()
    override val Sitelet = Site.Main

type Startup() =

    member this.ConfigureServices(services: IServiceCollection) = services.AddSitelet<Website>() |> ignore

    member this.Configure(app: IApplicationBuilder) =
        app.UseWebSharper().Run(fun context ->
           context.Response.StatusCode <- 404
           context.Response.WriteAsync("Page not found"))

let args = System.Environment.GetCommandLineArgs()

Host.CreateDefaultBuilder(args)
    .ConfigureWebHost(fun webHost ->
        webHost.UseKestrel(fun c -> c.AddServerHeader <- false)
                .ConfigureLogging(fun config -> config.ClearProviders() |> ignore)
                .UseStartup<Startup>()
                |> ignore)
    .Build()
    .Run() 
