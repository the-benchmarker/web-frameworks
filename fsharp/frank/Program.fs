open Frank.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing

let home =
    resource "" {
        name "Hello Name"
        get (fun (ctx:HttpContext) -> ctx.Response.WriteAsync(""))
    }

let userId =
    resource "user/{id}" {
        name "User Id"
        get (fun (ctx:HttpContext) ->
            let userId = ctx.GetRouteValue("id")
            ctx.Response.WriteAsync(string userId))
    }

let user =
    resource "user" {
        name "User"
        post (fun (ctx:HttpContext) -> ctx.Response.WriteAsync(""))
    }

[<EntryPoint>]
let main args =
    let builder =
        webHost (WebHostBuilder().UseKestrel()) {
            resource home
            resource userId
            resource user
        }
    builder.Build().Run()
    0