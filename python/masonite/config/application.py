"""Application Settings."""

import os

from masonite import env

"""Application Name
This value is the name of your application. This value is used when the
framework needs to place the application's name in a notification or
any other location as required by the application or its packages.
"""

NAME = env("APP_NAME", "Masonite 3.0")

"""Application Debug Mode
When your application is in debug mode, detailed error messages with
stack traces will be shown on every error that occurs within your
application. If disabled, a simple generic error page is shown
"""

DEBUG = env("APP_DEBUG", False)

"""Secret Key
This key is used to encrypt and decrypt various values. Out of the box
Masonite uses this key to encrypt or decrypt cookies so you can use
it to encrypt and decrypt various values using the Masonite Sign
class. Read the documentation on Encryption to find out how.
"""

KEY = env("KEY", None)

"""Application URL
Sets the root URL of the application. This is primarily used for testing
"""

URL = env("APP_URL", "http://localhost:8000")

"""Base Directory
Sets the root path of your project
"""

BASE_DIRECTORY = os.getcwd()

"""Static Root
Set the static root of your application that you wil use to store assets
"""

STATIC_ROOT = os.path.join(BASE_DIRECTORY, "storage")

"""Autoload Directories
List of directories that are used to find classes and autoload them into
the Service Container. This is initially used to find models and load
them in but feel free to autoload any directories
"""

AUTOLOAD = [
    "app",
]
