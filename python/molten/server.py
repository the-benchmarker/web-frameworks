from molten import App, Route, HTTP_200, Response


def index() -> str:
    return Response(HTTP_200, content="")


def get_user(user_id: int) -> str:
    return Response(HTTP_200, content=f"{user_id}")


def create_user() -> str:
    return Response(HTTP_200, content="")


app = App(
    routes=[
        Route("/", index),
        Route("/user/{user_id}", get_user),
        Route("/user", create_user, method="POST"),
    ]
)
