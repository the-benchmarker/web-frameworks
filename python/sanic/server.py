"""
Sanic Benchmark Server - Production-Grade Implementation

A high-performance benchmark server implementation using Sanic framework.
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
import multiprocessing
import os
import sys
from typing import Any, Dict

from sanic import Request, Sanic
from sanic.exceptions import SanicException
from sanic.response import text

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.sanic")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("sanic").setLevel(logging.WARNING)

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


def add_security_headers(response: Any) -> Any:
    """
    Add security headers to a response.
    
    Args:
        response: The Sanic response object.
        
    Returns:
        Any: The response with security headers added.
    """
    for header, value in SECURITY_HEADERS.items():
        response.headers[header] = value
    return response

# Create Sanic application with optimized settings
app = Sanic("benchmark")

# Configure for production
app.config.FALLBACK_ERROR_FORMAT = "text"
app.config.RESPONSE_TIMEOUT = 300  # 5 minutes
app.config.REQUEST_MAX_SIZE = 16 * 1024 * 1024  # 16 MB
app.config.KEEPALIVE_TIMEOUT = 75
app.config.KEEPALIVE = True
app.config.DEBUG = DEBUG_MODE

# Add security headers middleware
@app.middleware("response")
async def add_security_middleware(request: Request, response: Any) -> Any:
    """
    Middleware to add security headers to all responses.
    
    Args:
        request: The Sanic Request object.
        response: The Sanic response object.
        
    Returns:
        Any: The response with security headers added.
    """
    return add_security_headers(response)


@app.route("/", methods=["GET"])
async def index(request: Request) -> Any:
    """
    Root endpoint handler.
    
    Args:
        request: Sanic Request object.
    
    Returns:
        text: Empty response with security headers.
    """
    logger.debug("Root endpoint accessed")
    return text("", status=200)


@app.route("/user/<id:int>", methods=["GET"])
async def get_user(request: Request, id: int) -> Any:
    """
    Retrieve user information by ID.
    
    Args:
        request: Sanic Request object.
        id: The user identifier.
    
    Returns:
        text: The user ID as plain text.
        
    Raises:
        HTTPBadRequest: If ID is invalid (security validation).
    """
    # Security: Validate input - reject non-positive IDs
    if id <= 0:
        if DEBUG_MODE:
            logger.debug(f"Invalid user ID: {id}")
        raise SanicException("Invalid ID parameter", status_code=400)
    
    logger.debug(f"User endpoint accessed with ID: {id}")
    return text(str(id), status=200)


@app.route("/user", methods=["POST"])
async def create_user(request: Request) -> Any:
    """
    Create a new user.
    
    Args:
        request: Sanic Request object.
    
    Returns:
        text: Empty response with 201 status and security headers.
    """
    logger.debug("Create user endpoint accessed")
    
    # Security: Return 201 Created for POST requests
    return text("", status=201)


@app.route("/health", methods=["GET"])
async def health_check(request: Request) -> Any:
    """
    Health check endpoint for monitoring.
    
    Args:
        request: Sanic Request object.
    
    Returns:
        text: Simple health status with security headers.
    """
    return text("OK", status=200)


@app.route("/error", methods=["GET"])
async def trigger_error(request: Request) -> Any:
    """
    Endpoint to trigger an error for testing error handling.
    
    Args:
        request: Sanic Request object.
    
    Returns:
        Any: This should not be reached as it raises an exception.
    """
    raise RuntimeError("Test error for error handling")


# Global exception handler
@app.exception(SanicException)
async def handle_sanic_exception(request: Request, exception: SanicException) -> Any:
    """
    Global exception handler for Sanic-specific exceptions.
    
    Args:
        request: Sanic Request object.
        exception: The Sanic exception that was raised.
    
    Returns:
        text: Error response.
    """
    if DEBUG_MODE:
        logger.exception(f"Sanic exception: {exception}")
    else:
        logger.warning(f"Sanic exception: {type(exception).__name__}")
    
    return text(str(exception) if DEBUG_MODE else "Internal Server Error", status=exception.status_code)


@app.exception(BaseException)
async def handle_general_exception(request: Request, exception: Exception) -> Any:
    """
    Global exception handler for all other exceptions.
    
    Args:
        request: Sanic Request object.
        exception: The exception that was raised.
    
    Returns:
        text: Error response.
    """
    if DEBUG_MODE:
        logger.exception(f"Unhandled exception: {exception}")
    else:
        logger.warning(f"Unhandled exception: {type(exception).__name__}")
    
    # Don't expose internal error details in production
    return text("Internal Server Error", status=500)


# 404 handler
@app.route("/<path:path>")
async def not_found(request: Request, path: str) -> Any:
    """
    Handle 404 Not Found.
    
    Args:
        request: Sanic Request object.
        path: The requested path.
    
    Returns:
        text: Not found response.
    """
    if DEBUG_MODE:
        logger.debug(f"404 Not Found: {path}")
    return text("Not Found", status=404)


if __name__ == "__main__":
    import sys

    # Get configuration from environment or use defaults
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    workers = int(os.getenv("WORKERS", multiprocessing.cpu_count()))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Sanic benchmark server in production mode on {host}:{port} with {workers} workers")
    else:
        logger.info(f"Starting Sanic benchmark server on {host}:{port} with {workers} workers")
    
    app.run(
        host=host,
        port=port,
        workers=workers,
        debug=DEBUG_MODE,
        access_log=DEBUG_MODE,  # Disable access logs in production for performance
        auto_reload=DEBUG_MODE,
    )
