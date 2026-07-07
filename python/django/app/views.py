"""
Django Benchmark Views

Benchmark endpoint views following Django best practices.
Uses type hints, proper error handling, and logging.
"""

from __future__ import annotations

import logging
from typing import Any

from django.http import HttpRequest, HttpResponse

# Configure module logger
logger = logging.getLogger("benchmark.django.views")


def index(request: HttpRequest) -> HttpResponse:
    """
    Root endpoint handler.
    
    Args:
        request: Django HttpRequest object.
    
    Returns:
        HttpResponse: Empty response with 200 status for benchmarking.
    """
    logger.debug("Root endpoint accessed")
    return HttpResponse(status=200, content="", content_type="text/plain")


def get_user(request: HttpRequest, id: int) -> HttpResponse:
    """
    Retrieve user information by ID.
    
    Args:
        request: Django HttpRequest object.
        id: The user identifier.
    
    Returns:
        HttpResponse: The user ID as plain text.
    """
    logger.debug(f"User endpoint accessed with ID: {id}")
    return HttpResponse(status=200, content=str(id), content_type="text/plain")


def create_user(request: HttpRequest) -> HttpResponse:
    """
    Create a new user.
    
    Args:
        request: Django HttpRequest object.
    
    Returns:
        HttpResponse: Empty response with 200 status for benchmarking.
    """
    logger.debug("Create user endpoint accessed")
    return HttpResponse(status=200, content="", content_type="text/plain")


def health_check(request: HttpRequest) -> HttpResponse:
    """
    Health check endpoint for monitoring.
    
    Args:
        request: Django HttpRequest object.
    
    Returns:
        HttpResponse: Simple health status.
    """
    return HttpResponse(status=200, content="OK", content_type="text/plain")


# Custom exception handler (if needed)
def custom_exception_handler(get_response: Any) -> Any:
    """
    Middleware for exception handling.
    
    Args:
        get_response: The next middleware or view.
    
    Returns:
        Function: Middleware function.
    """
    def middleware(request: HttpRequest) -> HttpResponse:
        try:
            return get_response(request)
        except Exception as error:
            logger.error(f"Unhandled exception: {error}", exc_info=True)
            return HttpResponse(
                status=500,
                content="Internal Server Error",
                content_type="text/plain",
            )
    
    return middleware
