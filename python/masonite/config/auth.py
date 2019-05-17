"""Authentication Settings."""

from masonite import env

from app.User import User

"""Authentication Model
Put the model here that will be used to authenticate users to your site.
Currently the model must contain a password field. In the model should
be an auth_column = 'column' in the Meta class. This column will be
used to verify credentials in the Auth facade or any other auth
classes. The auth_column will be used to change auth things
like 'email' to 'user' to easily switch which column will
be authenticated.

@see masonite.auth.Auth
"""

AUTH = {
    'driver': env('AUTH_DRIVER', 'cookie'),
    'model': User,
}
