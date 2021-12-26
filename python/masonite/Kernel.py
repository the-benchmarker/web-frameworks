from masonite.foundation import response_handler
from masonite.storage import StorageCapsule
from masonite.auth import Sign
from masonite.environment import LoadEnvironment
from masonite.utils.structures import load
from masonite.utils.location import base_path
from masonite.middleware import (
    SessionMiddleware,
    EncryptCookies,
    LoadUserMiddleware,
    MaintenanceModeMiddleware,
)
from masonite.routes import Route
from masonite.configuration.Configuration import Configuration
from masonite.configuration import config
from config.filesystem import STATICFILES

from app.middlewares.VerifyCsrfToken import VerifyCsrfToken


class Kernel:

    http_middleware = [MaintenanceModeMiddleware, EncryptCookies]

    route_middleware = {
        "web": [SessionMiddleware, LoadUserMiddleware, VerifyCsrfToken],
    }

    def __init__(self, app):
        self.application = app

    def register(self):
        # Register routes
        self.load_environment()
        self.register_configurations()
        self.register_middleware()
        self.register_routes()
        self.register_database()
        self.register_templates()
        self.register_storage()

    def load_environment(self):
        LoadEnvironment()

    def register_configurations(self):
        # load configuration
        self.application.bind("config.location", "config")
        configuration = Configuration(self.application)
        configuration.load()
        self.application.bind("config", configuration)
        key = config("application.key")
        self.application.bind("key", key)
        self.application.bind("sign", Sign(key))
        # set locations
        self.application.bind("resources.location", "resources/")
        self.application.bind("controllers.location", "app/controllers")
        self.application.bind("jobs.location", "app/jobs")
        self.application.bind("providers.location", "app/providers")
        self.application.bind("mailables.location", "app/mailables")
        self.application.bind("listeners.location", "app/listeners")
        self.application.bind("validation.location", "app/validation")
        self.application.bind("notifications.location", "app/notifications")
        self.application.bind("events.location", "app/events")
        self.application.bind("tasks.location", "app/tasks")
        self.application.bind("models.location", "app/models")
        self.application.bind("observers.location", "app/models/observers")
        self.application.bind("policies.location", "app/policies")
        self.application.bind("commands.location", "app/commands")
        self.application.bind("middlewares.location", "app/middlewares")

        self.application.bind("server.runner", "masonite.commands.ServeCommand.main")

    def register_middleware(self):
        self.application.make("middleware").add(self.route_middleware).add(self.http_middleware)

    def register_routes(self):
        Route.set_controller_locations(self.application.make("controllers.location"))
        self.application.bind("routes.location", "routes/web")
        self.application.make("router").add(
            Route.group(
                load(self.application.make("routes.location"), "ROUTES"), middleware=["web"]
            )
        )

    def register_database(self):
        from masoniteorm.query import QueryBuilder

        self.application.bind(
            "builder",
            QueryBuilder(connection_details=config("database.databases")),
        )

        self.application.bind("migrations.location", "databases/migrations")
        self.application.bind("seeds.location", "databases/seeds")

        self.application.bind("resolver", config("database.db"))

    def register_templates(self):
        self.application.bind("views.location", "templates/")

    def register_storage(self):
        storage = StorageCapsule()
        storage.add_storage_assets(STATICFILES)
        self.application.bind("storage_capsule", storage)

        self.application.set_response_handler(response_handler)
        self.application.use_storage_path(base_path("storage"))
