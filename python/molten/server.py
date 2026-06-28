"""
Molten Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Molten framework.
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

from molten import App, Route, HTTP_200, HTTP_201, HTTP_400, HTTP_500, Response

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.molten")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("molten").setLevel(logging.WARNING)
    logging.getLogger("waitress").setLevel(logging.WARNING)

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
        response: The Response object to add headers to.
        
    Returns:
        Response: The response with security headers added.
    """
    for header, value in SECURITY_HEADERS.items():
        response.headers[header] = value
    return response


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


def index() -> Response:
    """
    Root endpoint handler.
    
    Returns:
        Response: Empty response with security headers.
    """
    logger.debug("Root endpoint accessed")
    response = Response(HTTP_200, content="")
    add_security_headers(response)
    return response


def get_user(id: int) -> Response:
    """
    Retrieve user information by ID.
    
    Args:
        id: The user identifier from URL path.
        
    Returns:
        Response: Response containing the user ID with security headers.
        
    Raises:
        Response: If ID is invalid (security validation).
    """
    # Security: Validate input - reject non-positive IDs
    if id <= 0:
        if DEBUG_MODE:
            logger.debug(f"Invalid user ID: {id}")
        return Response(HTTP_400, content="Invalid ID parameter")
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {id}")
    
    response = Response(HTTP_200, content=str(id))
    add_security_headers(response)
    return response


def create_user() -> Response:
    """
    Create a new user.
    
    Returns:
        Response: Empty response with 201 status and security headers.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    
    # Security: Return 201 Created for POST requests
    response = Response(HTTP_201, content="")
    add_security_headers(response)
    return response


def health_check() -> Response:
    """
    Health check endpoint for monitoring.
    
    Returns:
        Response: Simple health status response with security headers.
    """
    response = Response(HTTP_200, content="OK")
    add_security_headers(response)
    return response


def trigger_error() -> Response:
    """
    Endpoint to trigger an error for testing error handling.
    
    Returns:
        Response: This should not be reached as it raises an exception.
    """
    raise RuntimeError("Test error for error handling")


# Global exception handler for Molten
def exception_handler(exc: Exception, request: Any) -> Response:
    """
    Global exception handler for Molten application.
    
    Args:
        exc: The exception that was raised.
        request: The request object.
        
    Returns:
        Response: Error response with security headers.
    """
    if DEBUG_MODE:
        logger.exception(f"Unhandled exception: {exc}")
    else:
        logger.warning(f"Unhandled exception: {type(exc).__name__}")
    
    response = Response(HTTP_500, content="Internal Server Error")
    add_security_headers(response)
    return response


# =============================================================================
# APPLICATION SETUP
# =============================================================================


app = App(
    routes=[
        Route("/", index),
        Route("/user/{id}", get_user),
        Route("/user", create_user, method="POST"),
        Route("/health", health_check, method="GET"),
        Route("/error", trigger_error, method="GET"),
    ],
    exception_handler=exception_handler,
)


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Molten benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Molten benchmark server on {host}:{port}")
    
    # Import and use waitress for production serving
    import waitress
    
    waitress.serve(
        app,
        host=host,
        port=port,
        threads=4,
        connection_limit=1000,
        cleanup_interval=30,
    )
