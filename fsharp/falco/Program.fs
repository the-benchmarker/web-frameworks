module Program 

open Falco

[<EntryPoint>]
let main args =       
    App.run [
        get  "/user/{id}" (Request.mapRoute (fun m -> m.["id"]) Response.ofPlainText)
        post "/user"      (Response.ofEmpty)
        get  "/"          (Response.ofEmpty)
    ]
    0