"""Load User Middleware."""

from masonite.auth import Auth
from masonite.request import Request


class LoadUserMiddleware:
    """Middleware class which loads the current user into the request."""

    def __init__(self, request: Request):
        """Inject Any Dependencies From The Service Container.

        Arguments:
            Request {masonite.request.Request} -- The Masonite request object.
        """
        self.request = request

    def before(self):
        """Run This Middleware Before The Route Executes."""
        self.load_user()
        return self.request

    def after(self):
        """Run This Middleware After The Route Executes."""
        pass

    def load_user(self):
        """Load user into the request.

        Arguments:
            request {masonite.request.Request} -- The Masonite request object.
        """
        self.request.set_user(Auth(self.request).user())
