"""
Pyramid Benchmark Server

A benchmark server implementation using Pyramid framework.
Follows Python best practices including type hints, proper configuration,
and clean separation of concerns.
"""

from __future__ import annotations

import logging
import os
from typing import Any

from pyramid.config import Configurator
from pyramid.exceptions import ExceptionResponse
from pyramid.request import Request
from pyramid.response import Response

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.pyramid")


def root_handler(request: Request) -> Response:
    """
    Root endpoint handler.
    
    Args:
        request: Pyramid Request object.
    
    Returns:
        Response: Empty response for benchmarking.
    """
    logger.debug("Root endpoint accessed")
    return Response(body=b"", content_type="text/plain")


def create_user_handler(request: Request) -> Response:
    """
    Create a new user.
    
    Args:
        request: Pyramid Request object.
    
    Returns:
        Response: Empty response for benchmarking.
    """
    logger.debug("Create user endpoint accessed")
    return Response(body=b"", content_type="text/plain")


def get_user_handler(request: Request) -> Response:
    """
    Retrieve user information by ID.
    
    Args:
        request: Pyramid Request object.
    
    Returns:
        Response: The user ID as plain text.
    """
    user_id = request.matchdict["id"]
    logger.debug(f"User endpoint accessed with ID: {user_id}")
    return Response(body=user_id.encode(), content_type="text/plain")


def health_check_handler(request: Request) -> Response:
    """
    Health check endpoint for monitoring.
    
    Args:
        request: Pyramid Request object.
    
    Returns:
        Response: Simple health status.
    """
    return Response(body=b"OK", content_type="text/plain")


def configure_app() -> Configurator:
    """
    Configure and return the Pyramid configurator.
    
    Returns:
        Configurator: Configured Pyramid configurator.
    """
    # Create configurator
    config = Configurator(
        # Production settings
        debug=False,
        # Request factory (if needed for custom request types)
        # authenticator (if needed)
        # authorization policy (if needed)
    )

    # Add routes
    config.add_route("root", "/", request_method="GET")
    config.add_route("create_user", "/user", request_method="POST")
    config.add_route("get_user", "/user/{id}", request_method="GET")
    config.add_route("health_check", "/health", request_method="GET")

    # Add views
    config.add_view(root_handler, route_name="root")
    config.add_view(create_user_handler, route_name="create_user")
    config.add_view(get_user_handler, route_name="get_user")
    config.add_view(health_check_handler, route_name="health_check")

    # Custom exception view (optional, for custom error responses)
    def exception_view(context: Any, request: Request) -> ExceptionResponse:
        """Custom exception view for error handling."""
        logger.error(f"Unhandled exception: {context.exception}", exc_info=True)
        return ExceptionResponse(
            status=int(context.status),
            detail=str(context.exception) if context.exception else "Internal Server Error",
            content_type="text/plain",
        )

    config.add_exception_view(exception_view)

    # Request methods (if needed for validation)
    # config.add_request_method(...)

    # Custom directives (if needed)
    # config.add_directive(...)

    return config


# Create application
config = configure_app()
app = config.make_wsgi_app()


# For standalone execution
if __name__ == "__main__":
    from wsgiref.simple_server import make_server

    # Get configuration from environment
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", "3000"))

    logger.info(f"Starting Pyramid benchmark server on {host}:{port}")
    
    # Run with simple WSGI server
    with make_server(host, port, app) as httpd:
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            logger.info("Server stopped by user")
