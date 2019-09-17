open Suave
open Suave.Filters
open Suave.Operators
open System.Net
let app : WebPart =
  choose [ 
    GET >=> path "/" >=> Successful.OK "";
    GET >=> pathScan "/user/%s" (fun s -> Successful.OK s);
    POST >=> path "/user" >=> Successful.OK  ""; ]

[<EntryPoint>]
let main argv = 
  startWebServer { defaultConfig with bindings = [ { scheme = HTTP ; socketBinding = { ip = IPAddress.Parse "0.0.0.0" ; port = 3000us }};] } app
  0
