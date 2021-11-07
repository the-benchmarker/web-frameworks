from masonite.routes import Route

ROUTES = [
    Route.get("/", "PageController@index"),
    Route.get("/user/@id:int", "PageController@show_user"),
    Route.post("/user", "PageController@create_user"),
]
