import os

import veil


@veil.get("/", offload=False)
def index():
    return ""


@veil.get("/user/{id}", offload=False)
def user_info(id: str):
    return id


@veil.post("/user", offload=False)
def user():
    return ""


veil.run(
    listen_cleartext="0.0.0.0:3000",
    threads=len(os.sched_getaffinity(0)),
    openapi=False,
    overrides={"access_log.enabled": False},
)
