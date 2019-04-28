"""Authentication Middleware."""

from masonite.request import Request


class AuthenticationMiddleware:
    """Middleware To Check If The User Is Logged In."""

    def __init__(self, request: Request):
        """Inject Any Dependencies From The Service Container.

        Arguments:
            Request {masonite.request.Request} -- The Masonite request object
        """
        self.request = request

    def before(self):
        """Run This Middleware Before The Route Executes."""
        if not self.request.user():
            self.request.redirect_to('login')

    def after(self):
        """Run This Middleware After The Route Executes."""
        pass
