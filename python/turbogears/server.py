from tg import MinimalApplicationConfigurator, RestController, TGController, expose


class UserController(RestController):
    @expose()
    def post(self):
        return ""

    @expose()
    def get_one(self, id):
        return id


class RootController(TGController):
    user = UserController()

    @expose()
    def index(self):
        return ""


config = MinimalApplicationConfigurator()
config.update_blueprint({"root_controller": RootController()})
app = config.make_wsgi_app()
