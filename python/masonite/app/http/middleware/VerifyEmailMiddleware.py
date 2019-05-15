"""Verify Email Middleware."""

from masonite.request import Request


class VerifyEmailMiddleware:
    """Middleware To Check If The User Has Verified Their Email."""

    def __init__(self, request: Request):
        """Inject Any Dependencies From The Service Container.

        Arguments:
            Request {masonite.request.Request} -- The Masonite request object
        """
        self.request = request

    def before(self):
        """Run This Middleware Before The Route Executes."""
        user = self.request.user()

        if user and user.verified_at is None:
            self.request.redirect('/email/verify')

    def after(self):
        """Run This Middleware After The Route Executes."""
        pass
