open Frank.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing
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
        post (fun _ -> Task.CompletedTask)
    }

let args = System.Environment.GetCommandLineArgs()

webHost args {
    configure (fun bldr -> bldr.UseKestrel(fun c -> c.AddServerHeader <- false).ConfigureLogging(fun c -> c.ClearProviders() |> ignore))
    resource home
    resource userId
    resource user
}
