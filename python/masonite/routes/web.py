from masonite.routes import Get, Post

ROUTES = [
    Get().route('/', 'PageController@index'),
    Get().route('/user/@id:int', 'PageController@show_user'),
    Post().route('/user', 'PageController@create_user'),
]
