module Program 

open Falco
open Falco.HostBuilder
open Falco.Routing
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging

let configureLogging (log : ILoggingBuilder) =
    log.ClearProviders()
    |> ignore

let configureServices (services : IServiceCollection) =
    services.AddFalco() 
    |> ignore

let configure (endpoints : HttpEndpoint list) (app : IApplicationBuilder) = 
    app.UseFalco(endpoints)       
    |> ignore 
    
let configureWebHost (endpoints : HttpEndpoint list) (webHost : IWebHostBuilder) =    
    webHost        
        .UseKestrel(fun k -> k.AddServerHeader <- false)
        .ConfigureLogging(configureLogging)
        .ConfigureServices(configureServices)
        .Configure(configure endpoints)        

let args = System.Environment.GetCommandLineArgs()

webHost args {
    configure configureWebHost
    endpoints [
        get  "/user/{id}" (Request.mapRoute (fun route -> route.Get "id" "") Response.ofPlainText)
        post "/user"      (Response.ofEmpty)
        get  "/"          (Response.ofEmpty)
    ]
}    
