open Suave
open Suave.Filters
open Suave.Logging
open Suave.Operators
open System.Net

let app: WebPart =
    choose
        [ GET >=> choose
                      [ path "/" >=> Successful.OK ""
                        pathScan "/user/%s" (fun s -> Successful.OK s) ]
          POST >=> path "/user" >=> Successful.OK "" ]

type NoopLogger() = 
    interface Logger with 
        member this.name : string[] = [|"null-logger"|]
        member this.logWithAck (logLevel : LogLevel) (logLevelWithMessage : (LogLevel -> Message)): Async<unit> =
            async {
                ()
            }
        member this.log (loglevel : LogLevel) (logLevelWithMessage : (LogLevel -> Message)): unit = 
            ()

let config =
    { defaultConfig with
        bindings =
            [ { scheme = HTTP
                socketBinding =
                    { ip = IPAddress.Parse "0.0.0.0"
                      port = 3000us } } ];
        logger = NoopLogger()}

startWebServer config app
