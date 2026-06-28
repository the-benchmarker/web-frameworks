(** Dream Benchmark Server

    A high-performance benchmark server implementation using the Dream framework.
    Follows OCaml best practices including proper error handling, logging, and configuration management.
*)

(** Load environment variables and configure server settings *)
let port = try int_of_string (Sys.getenv_opt "PORT" |> Option.value ~default:"3000") with _ -> 3000
let host = Sys.getenv_opt "HOST" |> Option.value ~default:"0.0.0.0"

(** Logging function for benchmarking *)
let log message = Printf.printf "[INFO] %s\n%!" message

(** Root endpoint handler
    Returns empty response with 200 OK status for benchmarking *)
let index _request =
  log "Root endpoint accessed";
  Dream.empty `Text

(** Get user by ID endpoint handler
    Returns user ID as plain text with 200 OK status
    
    @param request HTTP request containing path parameters
    @return HTTP response with user ID as body *)
let user_info request =
  let id = Dream.param request "id" in
  log (Printf.sprintf "User endpoint accessed with ID: %s" id);
  Dream.respond (Printf.sprintf "%s" id) ~status:`OK

(** Create user endpoint handler
    Returns empty response with 201 Created status for benchmarking *)
let create_user _request =
  log "Create user endpoint accessed";
  Dream.empty `Created

(** Health check endpoint for monitoring
    Returns "OK" with 200 OK status *)
let health_check _request =
  Dream.respond "OK" ~status:`OK

(** Custom error handler for 404 Not Found *)
let not_found _request =
  Dream.respond "Not Found" ~status:`Not_Found

(** Configure and run the Dream server *)
let () =
  let open Dream in
  run ~interface:host ~port
  @@ router [
       get "/" index;
       get "/user/:id" user_info;
       post "/user" create_user;
       get "/health" health_check;
       (* Catch-all for 404 *)
       any "" not_found;
     ]
