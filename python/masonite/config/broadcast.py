from masonite.environment import env


BROADCASTS = {
    "default": "pusher",
    "pusher": {
        "driver": "pusher",
        "client": env("PUSHER_CLIENT"),
        "app_id": env("PUSHER_APP_ID"),
        "secret": env("PUSHER_SECRET"),
        "cluster": env("PUSHER_CLUSTER"),
        "ssl": False,
    },
}
