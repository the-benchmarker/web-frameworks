from masonite.routes import Route


ROUTES = [
    Route.get("/", "UserController@index"),
    Route.get("/user/@id:int", "UserController@show"),
    Route.post("/user", "UserController@create"),
    Route.get("/health", "HealthController@index"),
    Route.get("/error", "ErrorController@index"),
]
