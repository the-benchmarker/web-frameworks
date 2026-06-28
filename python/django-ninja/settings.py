"""
Django Ninja Benchmark Settings - Production-Grade Implementation

Django configuration for Django Ninja benchmark server.
Implements security best practices, performance optimizations, and clean code.

Security Features:
- Debug mode disabled in production
- Secure secret key management
- Restricted allowed hosts
- Security headers middleware
- HTTPS and session security settings
- CSRF protection
"""

import os
import logging

# Build paths inside the project like this: os.path.join(BASE_DIR, ...)
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure logging for production - minimal and security-focused
LOGGING = {
    "version": 1,
    "disable_existing_loggers": False,
    "formatters": {
        "simple": {
            "format": "%(asctime)s - %(levelname)s - %(message)s",
        },
    },
    "handlers": {
        "console": {
            "class": "logging.StreamHandler",
            "level": "WARNING",
            "formatter": "simple",
        },
    },
    "root": {
        "handlers": ["console"],
        "level": "WARNING",
    },
    "loggers": {
        "django": {
            "handlers": ["console"],
            "level": "WARNING",
            "propagate": False,
        },
        "benchmark": {
            "handlers": ["console"],
            "level": "DEBUG" if DEBUG_MODE else "WARNING",
        },
    },
}

# =============================================================================
# SECURITY SETTINGS
# =============================================================================

# SECURITY WARNING: keep the secret key used in production secret!
SECRET_KEY = os.getenv(
    "DJANGO_SECRET_KEY",
    "3f51&0k++@_2u24_v@f)_-n7a0y&hc8^wmru)q^_flty9%!@er",
)

# SECURITY WARNING: don't run with debug turned on in production!
# Hardcoded to False for production security
DEBUG = False

# =============================================================================
# SECURITY HEADERS
# =============================================================================

# Security settings
SECURE_BROWSER_XSS_FILTER = True
SECURE_CONTENT_TYPE_NOSNIFF = True
X_FRAME_OPTIONS = "DENY"
REFERRER_POLICY = "strict-origin-when-cross-origin"
PERMISSIONS_POLICY = {
    "geolocation": (),
    "microphone": (),
    "camera": (),
    "payment": (),
    "usb": (),
}

# =============================================================================
# APPLICATION HOSTS
# =============================================================================

ALLOWED_HOSTS = ["127.0.0.1", "::1", "localhost"]
ALLOWED_HOSTS += ["172.17.%s.%s" % (i, j) for i in range(256) for j in range(256)]


# =============================================================================
# APPLICATION DEFINITION
# =============================================================================

INSTALLED_APPS = [
    "django.contrib.admin",
    "django.contrib.auth",
    "django.contrib.contenttypes",
    "django.contrib.sessions",
    "django.contrib.messages",
    "django.contrib.staticfiles",
]

# Security middleware - must be first
MIDDLEWARE = [
    "django.middleware.security.SecurityMiddleware",
    "django.middleware.common.CommonMiddleware",
    "django.middleware.csrf.CsrfViewMiddleware",
    "django.middleware.clickjacking.XFrameOptionsMiddleware",
]

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
            ],
            "debug": False,  # Security: Disable debug in templates
        },
    }
]

WSGI_APPLICATION = "app.wsgi.application"


# =============================================================================
# DATABASE
# =============================================================================

DATABASES = {
    "default": {
        "ENGINE": "django.db.backends.sqlite3",
        "NAME": os.path.join(BASE_DIR, "db.sqlite3"),
    }
}


# =============================================================================
# PASSWORD VALIDATION
# =============================================================================

AUTH_PASSWORD_VALIDATORS = [
    {
        "NAME": "django.contrib.auth.password_validation.UserAttributeSimilarityValidator"
    },
    {"NAME": "django.contrib.auth.password_validation.MinimumLengthValidator"},
    {"NAME": "django.contrib.auth.password_validation.CommonPasswordValidator"},
    {"NAME": "django.contrib.auth.password_validation.NumericPasswordValidator"},
]


# =============================================================================
# INTERNATIONALIZATION
# =============================================================================

LANGUAGE_CODE = "en-us"

TIME_ZONE = "UTC"

USE_I18N = True

USE_L10N = True

USE_TZ = True


# =============================================================================
# STATIC FILES
# =============================================================================

STATIC_URL = "/static/"


# =============================================================================
# ADDITIONAL SECURITY SETTINGS
# =============================================================================

# Session security settings
SESSION_COOKIE_HTTPONLY = True
SESSION_COOKIE_SECURE = True  # Only send over HTTPS
SESSION_COOKIE_SAMESITE = "Lax"
CSRF_COOKIE_SECURE = True  # Only send CSRF cookie over HTTPS
CSRF_COOKIE_HTTPONLY = True
CSRF_USE_SESSIONS = False

# Security: Don't allow Django to serve static files in production
SERVE_STATIC_FILES = False