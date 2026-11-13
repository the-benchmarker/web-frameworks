from cornice import Service
from pyramid.config import Configurator
from pyramid.response import Response

index = Service(name="index", path="/")
users = Service(name="users", path="/user")
user = Service(name="user", path="/user/{id}")


@index.get()
def get_index(request):
    return Response()


@users.post()
def create_user(request):
    return Response()


@user.get()
def get_user(request):
    return Response(request.matchdict["id"])


with Configurator() as config:
    config.include("cornice")
    config.add_cornice_service(index)
    config.add_cornice_service(users)
    config.add_cornice_service(user)
    app = config.make_wsgi_app()
