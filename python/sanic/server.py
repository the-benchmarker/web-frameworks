import multiprocessing

from sanic import Sanic
from sanic.response import text

LOGGING_CONFIG = dict(
    version=1,
    disable_existing_loggers=False,
    loggers={
        "sanic.root": {"level": "WARNING", "handlers": ["console"]},
        "sanic.error": {
            "level": "WARNING",
            "handlers": ["error_console"],
            "propagate": True,
            "qualname": "sanic.error",
        },
        "sanic.access": {
            "level": "WARNING",
            "handlers": ["access_console"],
            "propagate": True,
            "qualname": "sanic.access",
        },
    },
    handlers={
        "console": {
            "class": "logging.NullHandler",
            "formatter": "generic",
        },
        "error_console": {
            "class": "logging.NullHandler",
            "formatter": "generic",
        },
        "access_console": {
            "class": "logging.NullHandler",
            "formatter": "access",
        },
    },
    formatters={
        "generic": {
            "format": "%(asctime)s [%(process)d] [%(levelname)s] %(message)s",
            "datefmt": "[%Y-%m-%d %H:%M:%S %z]",
            "class": "logging.Formatter",
        },
        "access": {
            "format": "%(asctime)s - (%(name)s)[%(levelname)s][%(host)s]: "
            + "%(request)s %(message)s %(status)d %(byte)d",
            "datefmt": "[%Y-%m-%d %H:%M:%S %z]",
            "class": "logging.Formatter",
        },
    },
)


app = Sanic("benchmark", log_config=LOGGING_CONFIG)


@app.route("/")
async def index(request):
    return text("")


@app.route("/user/<id:int>", methods=["GET"])
async def user_info(request, id):
    return text(str(id))


@app.route("/user", methods=["POST"])
async def user(request):
    return text("")

if __name__ == "__main__":
    workers = multiprocessing.cpu_count()
    app.run(host='0.0.0.0', port=3000, workers=workers,
            debug=False, access_log=False)
