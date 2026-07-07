"""
Pyramid Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Pyramid framework.
Implements security best practices, performance optimizations, and clean code.

Security Features:
- Disabled debug mode and excessive logging
- Security headers on all responses
- Input validation
- Minimal error logging
- Proper HTTP status codes
"""

from __future__ import annotations

import logging
import os
import sys
from typing import Any, Dict

from pyramid.config import Configurator
from pyramid.exceptions import ExceptionResponse, HTTPBadRequest, HTTPNotFound, HTTPInternalServerError
from pyramid.request import Request
from pyramid.response import Response

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.pyramid")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("pyramid").setLevel(logging.WARNING)

# =============================================================================
# SECURITY HEADERS UTILITY
# =============================================================================

SECURITY_HEADERS: Dict[str, str] = {
    "X-Content-Type-Options": "nosniff",
    "X-Frame-Options": "DENY",
    "X-XSS-Protection": "1; mode=block",
    "Content-Security-Policy": "default-src 'self'",
    "Referrer-Policy": "strict-origin-when-cross-origin",
    "Cache-Control": "no-cache, no-store, must-revalidate",
}


def add_security_headers(response: Response) -> Response:
    """
    Add security headers to a response.
    
    Args:
        response: The Pyramid Response object.
        
    Returns:
        Response: The response with security headers added.
    """
    for header, value in SECURITY_HEADERS.items():
        response.headers[header] = value
    return response


def root_handler(request: Request) -> Response:
    """
    Root endpoint handler.
    
    Args:
        request: Pyramid Request object.
    
    Returns:
        Response: Empty response with security headers.
    """
    logger.debug("Root endpoint accessed")
    response = Response(body=b"", content_type="text/plain")
    add_security_headers(response)
    return response


def create_user_handler(request: Request) -> Response:
    """
    Create a new user.
    
    Args:
        request: Pyramid Request object.
    
    Returns:
        Response: Empty response with 201 status and security headers.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    
    # Security: Return 201 Created for POST requests
    response = Response(body=b"", content_type="text/plain", status=201)
    add_security_headers(response)
    return response


def get_user_handler(request: Request) -> Response:
    """
    Retrieve user information by ID.
    
    Args:
        request: Pyramid Request object.
    
    Returns:
        Response: The user ID as plain text with security headers.
        
    Raises:
        HTTPBadRequest: If ID is empty or invalid (security validation).
    """
    user_id = request.matchdict["id"]
    
    # Security: Validate input - reject empty IDs
    if not user_id or not user_id.strip():
        if DEBUG_MODE:
            logger.debug("Invalid user ID: empty")
        raise HTTPBadRequest(explanation="Missing or invalid ID parameter")
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {user_id}")
    
    response = Response(body=user_id.encode(), content_type="text/plain")
    add_security_headers(response)
    return response


def health_check_handler(request: Request) -> Response:
    """
    Health check endpoint for monitoring.
    
    Args:
        request: Pyramid Request object.
    
    Returns:
        Response: Simple health status with security headers.
    """
    response = Response(body=b"OK", content_type="text/plain")
    add_security_headers(response)
    return response


def error_handler(request: Request) -> Response:
    """
    Endpoint to trigger an error for testing error handling.
    
    Args:
        request: Pyramid Request object.
    
    Returns:
        Response: This should not be reached as it raises an exception.
    """
    raise RuntimeError("Test error for error handling")


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
    config.add_route("error", "/error", request_method="GET")

    # Add views
    config.add_view(root_handler, route_name="root")
    config.add_view(create_user_handler, route_name="create_user")
    config.add_view(get_user_handler, route_name="get_user")
    config.add_view(health_check_handler, route_name="health_check")
    config.add_view(error_handler, route_name="error")

    # Custom exception view for error handling
    def exception_view(context: Any, request: Request) -> ExceptionResponse:
        """Custom exception view for error handling."""
        if DEBUG_MODE:
            logger.exception(f"Unhandled exception: {context.exception}")
        else:
            logger.warning(f"Unhandled exception: {type(context.exception).__name__}")
        
        # Don't expose internal error details in production
        error_message = str(context.exception) if DEBUG_MODE and context.exception else "Internal Server Error"
        return ExceptionResponse(
            status=int(context.status),
            detail=error_message,
            content_type="text/plain",
        )

    config.add_exception_view(exception_view)

    # Add not found view
    def not_found_view(context: Any, request: Request) -> HTTPNotFound:
        """Custom 404 not found view."""
        if DEBUG_MODE:
            logger.debug(f"404 Not Found: {request.path}")
        return HTTPNotFound(explanation="Not Found")

    config.add_notfound_view(not_found_view)

    return config


# Create application
config = configure_app()
app = config.make_wsgi_app()


# For standalone execution
if __name__ == "__main__":
    # Import waitress for production serving
    import waitress
    
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Pyramid benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Pyramid benchmark server on {host}:{port}")
    
    # Run with waitress for production performance
    waitress.serve(
        app,
        host=host,
        port=port,
        threads=4,
        connection_limit=1000,
        cleanup_interval=30,
    )
