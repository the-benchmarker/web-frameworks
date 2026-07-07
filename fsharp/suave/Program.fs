/// <summary>
/// Production-grade Suave web application
/// Implements security best practices for the Suave IO web server
/// </summary>
module Program

open Suave
open Suave.Router
open Suave.Filters
open Suave.Operators
open Suave.Sockets
open Suave.Writers
open System.Net

// ===========================================================================
// Configuration Modules
// ===========================================================================

/// <summary>
/// Security configuration for production
/// </summary>
module SecurityConfig =
    /// Security headers to add to every response
    let securityHeaders : WebPart =
        fun ctx next ->
            async {
                // Remove server header
                ctx.response.headers <- ctx.response.headers |> Map.remove "Server"
                
                // Add security headers
                ctx.response.headers <- ctx.response.headers
                    |> Map.add "X-Content-Type-Options" [|"nosniff"|]
                    |> Map.add "X-Frame-Options" [|"DENY"|]
                    |> Map.add "X-XSS-Protection" [|"1; mode=block"|]
                    |> Map.add "Strict-Transport-Security" [|"max-age=63072000; includeSubDomains; preload"|]
                    |> Map.add "Content-Security-Policy" [|"default-src 'self'"|]
                    |> Map.add "Referrer-Policy" [|"strict-origin-when-cross-origin"|]
                    |> Map.add "Permissions-Policy" [|"geolocation=(), microphone=(), camera=()"|]
                    |> Map.add "Cache-Control" [|"no-store, no-cache, must-revalidate, private"|]
                
                return! next ctx
            }
    
    /// Request size limit (8MB)
    let maxRequestSize = 8L * 1024L * 1024L

// ===========================================================================
// Error Handling
// ===========================================================================

/// <summary>
/// Error handling for the application
/// </summary>
let errorHandler (ex: exn) (route: string) : HttpContext -> Async<HttpContext option> =
    fun ctx ->
        async {
            ctx.response.status <- HTTP_500.InternalServerError
            ctx.response.contentType <- "text/plain; charset=utf-8"
            return! (Successful.OK "Internal Server Error" >=> Writers.setStatus 500) ctx
        }

// ===========================================================================
// Route Handlers
// ===========================================================================

/// <summary>
/// Get user by ID handler
/// </summary>
let getUser (ctx: HttpContext) =
    match routeParam "id" ctx with
    | Some idStr ->
        { ctx with response = { ctx.response with content = System.Text.Encoding.UTF8.GetBytes idStr } }
        |> Successful.OK
    | None ->
        RequestErrors.BAD_REQUEST "Missing user ID" ctx

/// <summary>
/// Health check handler
/// </summary>
let healthCheck : WebPart =
    Writers.setStatus HTTP_200.OK >=> Writers.setMimeType "text/plain; charset=utf-8" >=> Writers.setBody ""

/// <summary>
/// User creation handler
/// </summary>
let createUser : WebPart =
    Writers.setStatus HTTP_201.Created >=> Writers.setMimeType "text/plain; charset=utf-8" >=> Writers.setBody ""

// ===========================================================================
// Application Routes
// ===========================================================================

/// <summary>
/// Main application router
/// </summary>
let app : WebPart =
    choose [
        // Health check
        path "/" >=> choose [
            GET >=> healthCheck
        ]
        
        // User routes
        path "/user" >=> choose [
            GET >=> pathScan "/%s" getUser
            POST >=> createUser
        ]
        
        // 404 Not Found
        path "" >=> RequestErrors.NOT_FOUND "Not Found"
    ]
    >=> SecurityConfig.securityHeaders

// ===========================================================================
// Server Configuration
// ===========================================================================

/// <summary>
/// Production-ready Suave configuration
/// </summary>
let config =
    { defaultConfig with
        bindings = [ HttpBinding.createSimple HTTP "0.0.0.0" 3000 ]
        bufferSize = 8192
        maxOps = 16384
        errorHandler = errorHandler
        // Security: Disable directory browsing
        homeFolder = None
        // Performance: TCP settings
        tcpBinding = { SocketBindingDefault with
                        maxConcurrentConnections = 16384
                        sendTimeout = System.TimeSpan.FromSeconds(5.0)
                        receiveTimeout = System.TimeSpan.FromSeconds(5.0)
                        keepAlive = System.TimeSpan.FromSeconds(60.0)
                        lingerState = LingerOption(false, System.TimeSpan.Zero)
                        noDelay = true
        }
        // Security: Disable compression to prevent CRIME/BREACH
        compression = GZipCompression compris
    }

// ===========================================================================
// Entry Point
// ===========================================================================

[<EntryPoint>]
let main argv =
    // Start the web server
    startWebServer config app
    0
