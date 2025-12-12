open Suave
open Suave.Router

let getUser (ctx: HttpContext) =
    match routeParam "id" ctx with
    | Some idStr ->
        Successful.OK idStr ctx
    | None ->
        RequestErrors.BAD_REQUEST "Missing user ID" ctx

let app  : WebPart = router {
    get "/" (Successful.OK "")
    get "/user/:id" getUser
    post "/user" (Successful.OK "")
}

let config =
    { defaultConfig with
        bindings =
            [ HttpBinding.createSimple HTTP "127.0.0.1" 3000 ];}

startWebServer config app
