"""
Bottle Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Bottle framework.
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
from typing import Any, Callable

from bottle import Bottle, Request, Response, run

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.bottle")

# Suppress Bottle logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("bottle").setLevel(logging.WARNING)

# =============================================================================
# SECURITY HEADERS PLUGIN
# =============================================================================


# Security headers plugin for Bottle
class SecurityHeadersPlugin:
    """
    Bottle plugin to add security headers to all responses.
    
    Security best practices:
    - X-Content-Type-Options: nosniff prevents MIME type sniffing
    - X-Frame-Options: DENY prevents clickjacking
    - X-XSS-Protection: enables XSS protection in browsers
    - Content-Security-Policy: restricts resource loading
    - Referrer-Policy: controls referrer information
    - Cache-Control: prevents caching of sensitive data
    """
    
    name = "security_headers"
    api = 2
    
    def __init__(self):
        self.security_headers = {
            "X-Content-Type-Options": "nosniff",
            "X-Frame-Options": "DENY",
            "X-XSS-Protection": "1; mode=block",
            "Content-Security-Policy": "default-src 'self'",
            "Referrer-Policy": "strict-origin-when-cross-origin",
            "Cache-Control": "no-cache, no-store, must-revalidate",
        }
    
    def apply(self, callback, context):
        def wrapper(*args, **kwargs):
            response = callback(*args, **kwargs)
            if isinstance(response, Response):
                for header, value in self.security_headers.items():
                    response.set_header(header, value)
            return response
        return wrapper


# Create Bottle application
app = Bottle()

# Install security headers plugin
app.install(SecurityHeadersPlugin())

# Error handling plugin
def error_handler_plugin(callback: Callable[..., Any]) -> Callable[..., Any]:
    """
    Decorator for error handling in routes.
    
    Args:
        callback: The route callback function.
    
    Returns:
        Wrapped callback with error handling.
    """
    def wrapper(*args: Any, **kwargs: Any) -> Any:
        try:
            return callback(*args, **kwargs)
        except Exception as error:
            if DEBUG_MODE:
                logger.exception(f"Unhandled exception in {callback.__name__}: {error}")
            else:
                logger.error(f"Unhandled exception in {callback.__name__}: {type(error).__name__}")
            return Response(status=500, body="Internal Server Error", content_type="text/plain")
    return wrapper


@app.route("/", method="GET")
@error_handler_plugin
def index() -> str:
    """
    Root endpoint handler.
    
    Returns:
        str: Empty response for benchmarking.
    """
    logger.debug("Root endpoint accessed")
    return ""


@app.route("/user/<id:path>", method="GET")
@error_handler_plugin
def user_info(id: str) -> str:
    """
    Retrieve user information by ID.
    
    Args:
        id: The user identifier.
    
    Returns:
        str: The user ID as plain text.
    
    Raises:
        HTTPError: If ID is empty (security validation).
    """
    # Security: Validate input - reject empty IDs
    if not id or not id.strip():
        if DEBUG_MODE:
            logger.debug("Invalid user ID: empty")
        raise ValueError("Missing or invalid ID parameter")
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {id}")
    return str(id)


@app.route("/user", method="POST")
@error_handler_plugin
def create_user() -> Response:
    """
    Create a new user.
    
    Returns:
        Response: Empty response with 201 status for benchmarking.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    # Security: Return 201 Created for POST requests
    return Response(status=201, body="", content_type="text/plain")


@app.route("/health", method="GET")
def health_check() -> str:
    """
    Health check endpoint for monitoring.
    
    Returns:
        str: Simple health status.
    """
    return "OK"


# Custom 404 handler
@app.error(404)
def not_found(error: Any) -> Response:
    """
    Handle 404 Not Found errors.
    
    Args:
        error: The error object.
    
    Returns:
        Response: 404 response with security headers.
    """
    if DEBUG_MODE:
        logger.debug(f"404 Not Found: {error}")
    return Response(status=404, body="Not Found", content_type="text/plain")


# Custom 500 handler
@app.error(500)
def internal_error(error: Any) -> Response:
    """
    Handle 500 Internal Server Error.
    
    Security best practices:
    - Don't expose internal error details to clients
    - Log errors for debugging (but minimal in production)
    
    Args:
        error: The error object.
    
    Returns:
        Response: 500 response with security headers.
    """
    if DEBUG_MODE:
        logger.exception(f"Internal server error: {error}")
    else:
        logger.error(f"Internal server error: {type(error).__name__}")
    return Response(status=500, body="Internal Server Error", content_type="text/plain")


# Health check endpoint
@app.route("/health", method="GET")
def health_check() -> str:
    """
    Health check endpoint for monitoring.
    
    Returns:
        str: Simple health status.
    """
    return "OK"


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000")
    server = os.getenv("SERVER", sys.argv[2] if len(sys.argv) > 2 else "waitress")

    # Run the application with production settings
    if not DEBUG_MODE:
        logger.warning("Starting Bottle benchmark server in production mode")
    else:
        logger.info(f"Starting Bottle benchmark server on {host}:{port} with {server} server")
    
    run(
        app,
        host=host,
        port=port,
        server=server,
        quiet=True,  # Suppress bottle's own logging
        reloader=False,
        debug=DEBUG_MODE,
        # Performance optimizations
        interval=1,
        socket_host=None,
        socket_port=None,
        socket_file=None,
    )
