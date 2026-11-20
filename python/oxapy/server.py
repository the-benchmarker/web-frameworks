import multiprocessing
from oxapy import Oxapy, Router, get, post


@get("/")
def index(_):
    return ""


@get("/user/{id:int}")
def get_user(_, id: int):
    return str(id)


@post("/user")
def create_user(_):
    return ""


def main():
    cpu_count = multiprocessing.cpu_count()
    (
        Oxapy(("0.0.0.0", 3000))
        .max_connections(1024)
        .channel_capacity(1024)
        .attach(Router().routes([index, get_user, create_user]))
        .run(workers=1, processes=cpu_count)
    )


if __name__ == "__main__":
    main()
