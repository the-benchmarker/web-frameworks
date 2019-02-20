from molten import App, Route, HTTP_200, Response


def index() -> str:
    return Response(HTTP_200, content="")


def get_user(id: int) -> str:
    return Response(HTTP_200, content=f"{id}")


def create_user() -> str:
    return Response(HTTP_200, content="")


app = App(
    routes=[
        Route("/", index),
        Route("/user/{id}", get_user),
        Route("/user", create_user, method="POST"),
    ]
)
