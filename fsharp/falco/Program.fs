open Falco
open Microsoft.AspNetCore.Http

let homeHandler = textOut ""

let userIdHandler =
    fun next (ctx: HttpContext) ->
        let userId = ctx.TryGetRouteValue "id" |> Option.defaultValue ""
        textOut userId next ctx

let userHandler = textOut ""

[<EntryPoint>]
let main args =
    webApp {
        get "/" homeHandler
        get "/user/{id}" userIdHandler
        post "/user" userHandler
    }
    0
