"""
Django Benchmark Settings

Django configuration for benchmark server.
Follows Django best practices for production settings.
"""

import os
from pathlib import Path

# Build paths inside the project like this: BASE_DIR / ...
BASE_DIR: Path = Path(__file__).resolve().parent.parent


# SECURITY WARNING: keep the secret key used in production secret!
# For benchmarking, we use a static key (not suitable for production)
SECRET_KEY: str = os.getenv(
    "DJANGO_SECRET_KEY",
    "3f51&0k++@_2u24_v@f)_-n7a0y&hc8^wmru)q^_flty9%!@er",
)

# SECURITY WARNING: don't run with debug turned on in production!
DEBUG: bool = os.getenv("DJANGO_DEBUG", "False").lower() == "true"

# Application hosts configuration
# For benchmarking, allow all hosts in Docker network ranges
ALLOWED_HOSTS: list[str] = [
    "127.0.0.1",
    "::1",
    "localhost",
]

# Docker default bridge network (172.17.0.0/16)
ALLOWED_HOSTS.extend(
    f"172.17.{i}.{j}" for i in range(256) for j in range(256)
)


# Application definition

INSTALLED_APPS = []

MIDDLEWARE = []

ROOT_URLCONF = "app.urls"

TEMPLATES = [
    {
        "BACKEND": "django.template.backends.django.DjangoTemplates",
        "DIRS": [],
        "APP_DIRS": True,
        "OPTIONS": {
            "context_processors": [
                "django.template.context_processors.debug",
                "django.template.context_processors.request",
                "django.contrib.auth.context_processors.auth",
                "django.contrib.messages.context_processors.messages",
            ]
        },
    }
]

WSGI_APPLICATION = "app.wsgi.application"


# Database
# https://docs.djangoproject.com/en/1.11/ref/settings/#databases

DATABASES = {
    "default": {
        "ENGINE": "django.db.backends.sqlite3",
        "NAME": os.path.join(BASE_DIR, "db.sqlite3"),
    }
}


# Password validation
# https://docs.djangoproject.com/en/1.11/ref/settings/#auth-password-validators

AUTH_PASSWORD_VALIDATORS = [
    {
        "NAME": "django.contrib.auth.password_validation.UserAttributeSimilarityValidator"
    },
    {"NAME": "django.contrib.auth.password_validation.MinimumLengthValidator"},
    {"NAME": "django.contrib.auth.password_validation.CommonPasswordValidator"},
    {"NAME": "django.contrib.auth.password_validation.NumericPasswordValidator"},
]


# Internationalization
# https://docs.djangoproject.com/en/1.11/topics/i18n/

LANGUAGE_CODE = "en-us"

TIME_ZONE = "UTC"

USE_I18N = True

USE_L10N = True

USE_TZ = True


# Static files (CSS, JavaScript, Images)
# https://docs.djangoproject.com/en/1.11/howto/static-files/

STATIC_URL = "/static/"
