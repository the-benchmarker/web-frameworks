#
# This file is automatically imported by the Supranim framework.
# It is used to define the routes for the application.
#

routes:
  get "/"
    # GET route links to `getHomepage` controller
  get "/user/{id:id}"
    # GET route with dynamic parameter `id` of type `int`
  post "/user"
    # POST route for creating a new user