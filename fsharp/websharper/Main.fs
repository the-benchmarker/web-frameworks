namespace web

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open WebSharper
open WebSharper.AspNetCore
open WebSharper.Sitelets

type EndPoint =
    | [<EndPoint "GET /">] Home
    | [<EndPoint "GET /user">] GetUser of id: string
    | [<EndPoint "POST /user">] User

module Site =

    let Main =
        Application.MultiPage(fun ctx endpoint ->
            match endpoint with
            | Home -> Content.Text ""
            | GetUser id -> Content.Text(id.Trim())
            | User -> Content.Text ""
            |> Content.WithContentType "text/plain")

type Website(config: IConfiguration) =
    inherit SiteletService<EndPoint>()
    override val Sitelet = Site.Main

type Startup() =

    member this.ConfigureServices(services: IServiceCollection) = services.AddSitelet<Website>() |> ignore

    member this.Configure(app: IApplicationBuilder) =
        app.UseWebSharper().Run(fun context ->
           context.Response.StatusCode <- 404
           context.Response.WriteAsync("Page not found"))

module Program =

    [<EntryPoint>]
    let main args =
        WebHostBuilder().UseKestrel().UseStartup<Startup>().Build().Run()
        0
