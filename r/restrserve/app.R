
# load_libraries ----------------------------

library("RestRserve")

# Source_Files ------------------------------

source("R/Functions.R")

# create app --------------------------------

app = Application$new()

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

backend = BackendRserve$new()
backend$start(app, http_port = 3000)
