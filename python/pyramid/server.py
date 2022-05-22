from pyramid.config import Configurator
from pyramid.response import Response


def empty(request):
    return Response()


def show(request):
    return Response(request.matchdict["id"])


with Configurator() as config:
    config.add_route("index", "/")
    config.add_route("create_user", "/user", request_method="POST")
    config.add_route("get_user", "/user/{id}")
    config.add_view(empty, route_name="index")
    config.add_view(empty, route_name="create_user")
    config.add_view(show, route_name="get_user")
    app = config.make_wsgi_app()
