import multiprocessing
from oxapy import Oxapy, Router, get, post


def main():
    cpu_count = multiprocessing.cpu_count()
    (
        Oxapy(("0.0.0.0", 3000))
        .attach(
            Router()
            .route(get("/", lambda _: ""))
            .route(get("/user/{id:int}", lambda _, id: str(id)))
            .route(post("/user", lambda _: ""))
        )
        .run(processes=cpu_count)
    )


if __name__ == "__main__":
    main()
