from masonite.routes import Get, Post

ROUTES = [
    Get("/", "PageController@index"),
    Get("/user/@id:int", "PageController@show_user"),
    Post("/user", "PageController@create_user"),
]
