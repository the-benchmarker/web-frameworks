from apidaora import appdaora, route
from apidaora.asgi.router import Route, make_router
from apidaora.asgi.app import asgi_app
from apidaora.method import MethodType
from apidaora.asgi.responses import PLAINTEXT_RESPONSE


def index(path, query, headers, body) -> str:
    return PLAINTEXT_RESPONSE, b""


def create(path, query, headers, body) -> str:
    return PLAINTEXT_RESPONSE, b""


def show(path, query, headers, body) -> str:
    return PLAINTEXT_RESPONSE, bytes(path.get('id'), encoding='utf-8')


routes = (
    Route("/", MethodType.GET, index),
    Route("/user", MethodType.POST, create),
    Route("/user/{id}", MethodType.GET, show),
)
router = make_router(routes)
app = asgi_app(router)
