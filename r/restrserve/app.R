# func_ServeMain ----------------------------

serve_main <- function(req, res) {
  res$set_body("")
}

# func_UserId ----------------------------

get_userid <- function(req, res) {
  id <- req$parameters_path
  res$set_body(id)
}

# func_postuser ----------------------------

post_user <- function(req, res) {
  res$set_body("")
}

# create app --------------------------------

app = RestRserve::Application$new()

# define Routes -----------------------------

### / main Route
app$add_get(
  path = "/", 
  FUN = serve_main,
  match = "exact"
)

### /user/:id get id
app$add_get(
  path = "/user/{id}",
  FUN = get_userid,
  match = "regex"
)

### /user post route
app$add_post(
  path = "/user",
  FUN = post_user,
  match = "exact"
)

# Run App -----------------------------------

backend = RestRserve::BackendRserve$new()
backend$start(app, http_port = 3000)
