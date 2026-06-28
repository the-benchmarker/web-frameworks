"""
Bottle Benchmark Server

A benchmark server implementation using Bottle framework.
Follows Python best practices including type hints, proper error handling, and logging.
"""

from __future__ import annotations

import logging
import os
import sys
from typing import Any, Callable

from bottle import Bottle, Request, Response, run

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.bottle")

# Create Bottle application
app = Bottle()

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
            logger.error(f"Unhandled exception in {callback.__name__}: {error}", exc_info=True)
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


@app.route("/user/<id:int>", method="GET")
@error_handler_plugin
def user_info(id: int) -> str:
    """
    Retrieve user information by ID.
    
    Args:
        id: The user identifier.
    
    Returns:
        str: The user ID as plain text.
    """
    logger.debug(f"User endpoint accessed with ID: {id}")
    return str(id)


@app.route("/user", method="POST")
@error_handler_plugin
def create_user() -> str:
    """
    Create a new user.
    
    Returns:
        str: Empty response for benchmarking.
    """
    logger.debug("Create user endpoint accessed")
    return ""


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
        Response: 404 response.
    """
    return Response(status=404, body="Not Found", content_type="text/plain")


# Custom 500 handler
@app.error(500)
def internal_error(error: Any) -> Response:
    """
    Handle 500 Internal Server Error.
    
    Args:
        error: The error object.
    
    Returns:
        Response: 500 response.
    """
    logger.error(f"Internal server error: {error}", exc_info=True)
    return Response(status=500, body="Internal Server Error", content_type="text/plain")


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000")
    server = os.getenv("SERVER", sys.argv[2] if len(sys.argv) > 2 else "waitress")

    # Run the application
    logger.info(f"Starting Bottle benchmark server on {host}:{port} with {server} server")
    run(
        app,
        host=host,
        port=port,
        server=server,
        quiet=False,
        reloader=False,
        debug=False,
    )
