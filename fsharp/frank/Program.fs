open Frank.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open System.Threading.Tasks

let home =
    resource "/" {
        name "Hello Name"
        get (fun _ -> Task.CompletedTask)
    }

let userId =
    resource "/user/{id}" {
        name "User Id"
        get (fun (ctx: HttpContext) ->
                let userId = ctx.GetRouteValue("id")
                ctx.Response.WriteAsync(string userId))
    }

let user =
    resource "/user" {
        name "User"
        get (fun _ -> Task.CompletedTask)
    }

[<EntryPoint>]
let main args =
    let builder =
        webHost (WebHostBuilder().UseKestrel(fun c -> c.AddServerHeader <- false).ConfigureLogging(fun c -> c.ClearProviders() |> ignore)) {
            resource home
            resource userId
            resource user
        }
    builder.Build().Run()
    0
