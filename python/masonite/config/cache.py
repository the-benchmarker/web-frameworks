from masonite.environment import env


STORES = {
    "default": "local",
    "local": {
        "driver": "file",
        "location": "storage/framework/cache"
        #
    },
    "redis": {
        "driver": "redis",
        "host": "127.0.0.1",
        "port": "6379",
        "password": "",
        "name": "project_name",
    },
    "memcache": {
        "driver": "memcache",
        "host": "127.0.0.1",
        "port": "11211",
        "password": "",
        "name": "project_name",
    },
}
