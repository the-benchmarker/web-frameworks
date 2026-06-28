"""
Guillotina Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Guillotina framework.
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

from guillotina import configure
from guillotina.factory import make_app
from guillotina.interfaces import IApplication
from guillotina.response import Response

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.guillotina")

# Suppress guillotina and related logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("guillotina").setLevel(logging.WARNING)
    logging.getLogger("aiohttp").setLevel(logging.WARNING)
    logging.getLogger("uvloop").setLevel(logging.WARNING)

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


@configure.service(method="GET", context=IApplication, permission="guillotina.Public")
async def index(context: Any, request: Any) -> Response:
    """
    Root endpoint handler.
    
    Args:
        context: The context object.
        request: The request object.
        
    Returns:
        Response: Empty response with security headers.
    """
    logger.debug("Root endpoint accessed")
    response = Response(body=b"")
    add_security_headers(response)
    return response


@configure.service(
    method="GET",
    context=IApplication,
    permission="guillotina.Public",
    name="/user/{id}",
)
async def user_info(context: Any, request: Any) -> Response:
    """
    Retrieve user information by ID.
    
    Args:
        context: The context object.
        request: The request object with matchdict containing 'id'.
        
    Returns:
        Response: Response containing the user ID with security headers.
        
    Raises:
        ValueError: If ID is empty or invalid (security validation).
    """
    id_ = request.matchdict["id"]
    
    # Security: Validate input - reject empty IDs
    if not id_ or not id_.strip():
        if DEBUG_MODE:
            logger.debug("Invalid user ID: empty")
        raise ValueError("Missing or invalid ID parameter")
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {id_}")
    
    response = Response(body=id_.encode("utf-8"))
    add_security_headers(response)
    return response


@configure.service(
    method="POST", context=IApplication, permission="guillotina.Public", name="/user"
)
async def create_user(context: Any, request: Any) -> Response:
    """
    Create a new user.
    
    Args:
        context: The context object.
        request: The request object.
        
    Returns:
        Response: Empty response with 201 status and security headers.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    
    # Security: Return 201 Created for POST requests
    response = Response(body=b"", status=201)
    add_security_headers(response)
    return response


@configure.service(
    method="GET", 
    context=IApplication, 
    permission="guillotina.Public",
    name="/health"
)
async def health_check(context: Any, request: Any) -> Response:
    """
    Health check endpoint for monitoring.
    
    Args:
        context: The context object.
        request: The request object.
        
    Returns:
        Response: Simple health status response with security headers.
    """
    response = Response(body=b"OK")
    add_security_headers(response)
    return response


# =============================================================================
# ERROR HANDLERS
# =============================================================================


@configure.service(
    method="GET", 
    context=IApplication, 
    permission="guillotina.Public",
    name="/error"
)
async def trigger_error(context: Any, request: Any) -> Response:
    """
    Endpoint to trigger an error for testing error handling.
    
    Args:
        context: The context object.
        request: The request object.
        
    Returns:
        Response: This should not be reached as it raises an exception.
    """
    raise RuntimeError("Test error for error handling")


# Custom exception handler for Guillotina
async def handle_exception(request: Any, view: Any, exception: Exception) -> Response:
    """
    Global exception handler for Guillotina application.
    
    Args:
        request: The request object.
        view: The view that raised the exception.
        exception: The exception that was raised.
        
    Returns:
        Response: Error response with security headers.
    """
    if DEBUG_MODE:
        logger.exception(f"Unhandled exception: {exception}")
    else:
        logger.warning(f"Unhandled exception: {type(exception).__name__}")
    
    response = Response(body=b"Internal Server Error", status=500)
    add_security_headers(response)
    return response


# =============================================================================
# APPLICATION SETUP
# =============================================================================


# Configure application with production settings
app = make_app(settings={
    "applications": ["server"],
    "debug": DEBUG_MODE,
    "logging": {
        "level": "WARNING" if not DEBUG_MODE else "DEBUG",
        "format": "%(asctime)s - %(levelname)s - %(message)s",
    },
})


# Configure for production
if not DEBUG_MODE:
    # Disable debug mode explicitly
    app.settings["debug"] = False
    # Disable auto-reload
    app.settings["reload"] = False


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Guillotina benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Guillotina benchmark server on {host}:{port}")
    
    # Run the application
    # Guillotina typically runs with aiohttp or uvicorn
    # For benchmarking purposes, we'll use the standard run method
    from guillotina import run
    run(
        app,
        host=host,
        port=port,
        debug=DEBUG_MODE,
        reload=DEBUG_MODE,
    )
