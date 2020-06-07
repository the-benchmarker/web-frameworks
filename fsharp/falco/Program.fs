open Falco
open Microsoft.AspNetCore.Http

let userIdHandler =
    fun next (ctx: HttpContext) ->
        let userId = ctx.TryGetRouteValue "id" |> Option.defaultValue ""
        textOut userId next ctx

webApp {
    get "/" (textOut "")
    get "/user/{id}" userIdHandler
    post "/user" (textOut "")
}
