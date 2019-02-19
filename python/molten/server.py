from molten import App, Route


def index() -> str:
    return ""


def get_user(id: int) -> int:
    return id


def create_user() -> str:
    return ""


app = App(
    routes=[
        Route("/", index),
        Route("/user/{id}", get_user),
        Route("/user", create_user, method="POST"),
    ]
)
