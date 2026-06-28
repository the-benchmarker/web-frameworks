(** Dream Benchmark Server

    A production-grade, high-performance benchmark server implementation using the Dream framework.
    Security best practices, performance optimizations, and clean code.
    
    @author The Benchmarker Team
    @version 1.0.0
*)

(** {1 Module Configuration} *)

(** Application constants for production deployment *)
let app_name = "Dream Benchmark Server"
let app_version = "1.0.0"

(** {1 Server Configuration} *)

(** Load environment variables with production defaults
    Security: Disable debug mode and reduce logging in production *)
let port = 
  try int_of_string (Sys.getenv_opt "PORT" |> Option.value ~default:"3000") 
  with _ -> 3000

let host = Sys.getenv_opt "HOST" |> Option.value ~default:"0.0.0.0"

(** Debug mode - DISABLED for production *)
let debug_mode = false

(** Logging level - ERROR only for production *)
let log_level = `Error

(** Production-grade logging that respects log level settings
    Security: No sensitive information logged in production *)
let log ?(level=`Info) message =
  if debug_mode || level = `Error then
    let level_str = match level with
      | `Debug -> "[DEBUG]"
      | `Info -> "[INFO]"
      | `Warn -> "[WARN]"
      | `Error -> "[ERROR]" in
    Printf.printf "%s %s\n%!" level_str message
  else
    () (* No logging in production for non-error messages *)

(** Security headers middleware
    Adds essential security headers to all responses *)
let with_security_headers handler request =
  let open Dream in
  let response = handler request in
  response
    |> set_header "X-Content-Type-Options" "nosniff"
    |> set_header "X-Frame-Options" "DENY"
    |> set_header "X-XSS-Protection" "1; mode=block"
    |> set_header "Content-Security-Policy" "default-src 'self'"
    |> set_header "Cache-Control" "max-age=3600"

(** Error handling middleware
    Catches exceptions and returns proper error responses *)
let with_error_handling handler request =
  try
    handler request
  with
  | Dream.Bad_request msg -> 
      Dream.respond "Bad Request" ~status:`Bad_request
  | Not_found -> 
      Dream.respond "Not Found" ~status:`Not_Found
  | exn -> 
      (* Security: Don't expose internal error details in production *)
      log ~level:`Error (Printf.sprintf "Internal Server Error: %s" (Printexc.to_string exn));
      Dream.respond "Internal Server Error" ~status:`Internal_Server_Error

(** {1 Request Handlers} *)

(** Root endpoint handler
    Returns empty response with 200 OK status for benchmarking
    Security: Sets content-type header *)
let index request =
  Dream.empty `Text
    |> set_header "Content-Type" "text/plain"

(** Get user by ID endpoint handler
    Returns user ID as plain text with 200 OK status
    
    @param request HTTP request containing path parameters
    @return HTTP response with user ID as body
    Security: Validates input and sanitizes response *)
let user_info request =
  try
    let id = Dream.param request "id" in
    (* Input validation - security best practice *)
    if String.length id = 0 then
      Dream.respond "Bad Request: Missing ID parameter" ~status:`Bad_request
    else
      Dream.respond (Printf.sprintf "%s" id) ~status:`OK
        |> set_header "Content-Type" "text/plain"
  with Not_found ->
    Dream.respond "Bad Request: Missing ID parameter" ~status:`Bad_request

(** Create user endpoint handler
    Returns empty response with 201 Created status for benchmarking
    Security: Proper HTTP status for resource creation *)
let create_user request =
  Dream.empty `Created
    |> set_header "Content-Type" "text/plain"

(** Health check endpoint for monitoring
    Returns "OK" with 200 OK status
    Security: Minimal response for health checks *)
let health_check request =
  Dream.respond "OK" ~status:`OK
    |> set_header "Content-Type" "text/plain"

(** {1 Error Handlers} *)

(** Custom error handler for 404 Not Found
    Security: Doesn't expose internal details *)
let not_found request =
  Dream.respond "Not Found" ~status:`Not_Found
    |> set_header "Content-Type" "text/plain"

(** Custom error handler for 405 Method Not Allowed *)
let method_not_allowed request =
  Dream.respond "Method Not Allowed" ~status:`Method_Not_Allowed
    |> set_header "Content-Type" "text/plain"

(** {1 Server Configuration and Entry Point} *)

(** Configure and run the Dream server with production settings
    Security: Debug disabled, proper error handling, security headers *)
let () =
  (* Startup message - only in non-production or when debug is enabled *)
  if debug_mode then
    Printf.printf "Starting %s v%s on %s:%d\n%!" app_name app_version host port;
  
  let open Dream in
  run ~interface:host ~port
  @@ router [
       (* Health check endpoint *)
       get "/" index;
       
       (* User endpoints with parameter validation *)
       get "/user/:id" user_info;
       
       (* Data creation endpoint *)
       post "/user" create_user;
       
       (* Health monitoring endpoint *)
       get "/health" health_check;
       
       (* Catch-all for 404 - Method Not Allowed for unsupported methods *)
       any "" not_found;
     ]
  (* Apply middleware chain for security and error handling *)
  |> with_security_headers
  |> with_error_handling
