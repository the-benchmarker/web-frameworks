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
    let Text (msg: string) =
        Content.Custom(
            Headers = [Http.Header.Custom "content-type" "text/plain"],
            WriteBody = fun s ->
                let encoding = System.Text.Encoding.UTF8
                use w = new System.IO.StreamWriter(s, encoding)
                w.Write msg)

    let Main =
        Application.MultiPage(fun _ endpoint ->
            match endpoint with
            | Home -> Text ""
            | GetUser id -> Text(id)
            | User -> Text "")

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
