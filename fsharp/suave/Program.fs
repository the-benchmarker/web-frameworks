open Suave
open Suave.Filters
open Suave.Operators
open System.Net

let app: WebPart =
    choose
        [ GET >=> choose
                      [ path "/" >=> Successful.OK ""
                        pathScan "/user/%s" (fun s -> Successful.OK s) ]
          POST >=> path "/user" >=> Successful.OK "" ]

let config =
    { defaultConfig with
            bindings =
                [ { scheme = HTTP
                    socketBinding =
                        { ip = IPAddress.Parse "0.0.0.0"
                        port = 3000us } } ] }

startWebServer config app
