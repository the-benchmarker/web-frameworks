"""
Robyn Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Robyn framework.
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

from robyn import Robyn, Request, Response, WebSocket
from robyn.robyn import ROBYN_CONFIG

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.robyn")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("robyn").setLevel(logging.WARNING)

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


def add_security_headers(response: Response) -> None:
    """
    Add security headers to a response.
    
    Args:
        response: The Robyn Response object.
    """
    for header, value in SECURITY_HEADERS.items():
        response.headers[header] = value


# =============================================================================
# APPLICATION SETUP
# =============================================================================


app = Robyn(__file__)


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


@app.get("/")
async def index(request: Request) -> Response:
    """
    Root endpoint handler.
    
    Args:
        request: The Robyn Request object.
        
    Returns:
        Response: Empty response with security headers.
    """
    logger.debug("Root endpoint accessed")
    response = Response("")
    add_security_headers(response)
    return response


@app.get("/user/:id")
async def user_info(request: Request) -> Any:
    """
    Retrieve user information by ID.
    
    Args:
        request: The Robyn Request object.
        
    Returns:
        Any: The user ID or error response.
    """
    id_ = request.path_params.get("id")
    
    # Security: Validate input - reject empty IDs
    if not id_ or not id_.strip():
        if DEBUG_MODE:
            logger.debug("Invalid user ID: empty")
        response = Response("Invalid ID parameter", status_code=400)
        add_security_headers(response)
        return response
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {id_}")
    
    response = Response(id_)
    add_security_headers(response)
    return response


@app.post("/user")
async def create_user(request: Request) -> Response:
    """
    Create a new user.
    
    Args:
        request: The Robyn Request object.
        
    Returns:
        Response: Empty response with 201 status and security headers.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    
    # Security: Return 201 Created for POST requests
    response = Response("", status_code=201)
    add_security_headers(response)
    return response


@app.get("/health")
async def health_check(request: Request) -> Response:
    """
    Health check endpoint for monitoring.
    
    Args:
        request: The Robyn Request object.
        
    Returns:
        Response: Simple health status with security headers.
    """
    response = Response("OK")
    add_security_headers(response)
    return response


@app.get("/error")
async def trigger_error(request: Request) -> Any:
    """
    Endpoint to trigger an error for testing error handling.
    
    Args:
        request: The Robyn Request object.
        
    Returns:
        Any: This should not be reached as it raises an exception.
    """
    raise RuntimeError("Test error for error handling")


# =============================================================================
# ERROR HANDLERS
# =============================================================================


@app.before_request()
async def before_request(request: Request) -> None:
    """
    Before request middleware to add security headers.
    
    Args:
        request: The Robyn Request object.
    """
    # Add security headers to the request context for use in responses
    request.context["security_headers"] = SECURITY_HEADERS


@app.after_request()
async def after_request(request: Request, response: Response) -> None:
    """
    After request middleware to add security headers to responses.
    
    Args:
        request: The Robyn Request object.
        response: The Robyn Response object.
    """
    add_security_headers(response)


@app.exception_handler()
async def handle_exception(request: Request, exception: Exception) -> Response:
    """
    Global exception handler for Robyn application.
    
    Args:
        request: The Robyn Request object.
        exception: The exception that was raised.
        
    Returns:
        Response: Error response with security headers.
    """
    if DEBUG_MODE:
        logger.exception(f"Unhandled exception: {exception}")
    else:
        logger.warning(f"Unhandled exception: {type(exception).__name__}")
    
    # Don't expose internal error details in production
    response = Response("Internal Server Error", status_code=500)
    add_security_headers(response)
    return response


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Robyn benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Robyn benchmark server on {host}:{port}")
    
    # Configure Robyn for production
    ROBYN_CONFIG["host"] = host
    ROBYN_CONFIG["port"] = port
    ROBYN_CONFIG["log_level"] = "warning" if not DEBUG_MODE else "debug"
    ROBYN_CONFIG["workers"] = 4
    ROBYN_CONFIG["processes"] = 2
    
    app.start(
        host=host,
        port=port,
        log_level="warning" if not DEBUG_MODE else "debug",
        workers=4,
        processes=2,
    )
