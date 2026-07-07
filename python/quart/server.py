"""
Quart Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Quart framework.
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

from quart import Quart, Request, Response

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.quart")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("quart").setLevel(logging.WARNING)
    logging.getLogger("uvicorn").setLevel(logging.WARNING)
    logging.getLogger("hypercorn").setLevel(logging.WARNING)

# =============================================================================
# SECURITY HEADERS MIDDLEWARE
# =============================================================================

SECURITY_HEADERS: Dict[str, str] = {
    "X-Content-Type-Options": "nosniff",
    "X-Frame-Options": "DENY",
    "X-XSS-Protection": "1; mode=block",
    "Content-Security-Policy": "default-src 'self'",
    "Referrer-Policy": "strict-origin-when-cross-origin",
    "Cache-Control": "no-cache, no-store, must-revalidate",
}


@app.after_request
async def add_security_headers(response: Response) -> Response:
    """
    Add security headers to all responses.
    
    Args:
        response: The Quart Response object.
        
    Returns:
        Response: The response with security headers added.
    """
    for header, value in SECURITY_HEADERS.items():
        response.headers[header] = value
    return response


# =============================================================================
# APPLICATION SETUP
# =============================================================================


app = Quart(__name__)

# Configure application for production
app.config["DEBUG"] = DEBUG_MODE


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


@app.route("/")
async def index() -> str:
    """
    Root endpoint handler.
    
    Returns:
        str: Empty response.
    """
    logger.debug("Root endpoint accessed")
    return ""


@app.route("/user/<id>")
async def user_info(id: str) -> Any:
    """
    Retrieve user information by ID.
    
    Args:
        id: The user identifier from URL path.
        
    Returns:
        Any: The user ID as string.
        
    Raises:
        Response: If ID is empty or invalid (security validation).
    """
    # Security: Validate input - reject empty IDs
    if not id or not id.strip():
        if DEBUG_MODE:
            logger.debug("Invalid user ID: empty")
        return Response("Invalid ID parameter", status=400)
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {id}")
    
    return id


@app.route("/user", methods=["POST"])
async def create_user() -> str:
    """
    Create a new user.
    
    Returns:
        str: Empty response.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    
    # Security: Return 201 Created for POST requests
    return "", 201


@app.route("/health")
async def health_check() -> str:
    """
    Health check endpoint for monitoring.
    
    Returns:
        str: Simple health status.
    """
    return "OK"


@app.route("/error")
async def trigger_error() -> Any:
    """
    Endpoint to trigger an error for testing error handling.
    
    Returns:
        Any: This should not be reached as it raises an exception.
    """
    raise RuntimeError("Test error for error handling")


# =============================================================================
# ERROR HANDLERS
# =============================================================================


@app.errorhandler(404)
async def not_found(error: Any) -> Response:
    """
    Handle 404 Not Found errors.
    
    Args:
        error: The error object.
        
    Returns:
        Response: 404 response with security headers.
    """
    if DEBUG_MODE:
        logger.debug(f"404 Not Found: {error}")
    return Response("Not Found", status=404)


@app.errorhandler(500)
async def internal_error(error: Any) -> Response:
    """
    Handle 500 Internal Server Error.
    
    Args:
        error: The error object.
        
    Returns:
        Response: 500 response with security headers.
    """
    if DEBUG_MODE:
        logger.exception(f"Internal server error: {error}")
    else:
        logger.error(f"Internal server error: {type(error).__name__}")
    return Response("Internal Server Error", status=500)


@app.errorhandler(Exception)
async def handle_exception(error: Exception) -> Response:
    """
    Global exception handler for Quart application.
    
    Args:
        error: The exception that was raised.
        
    Returns:
        Response: Error response with security headers.
    """
    if DEBUG_MODE:
        logger.exception(f"Unhandled exception: {error}")
    else:
        logger.warning(f"Unhandled exception: {type(error).__name__}")
    
    # Don't expose internal error details in production
    return Response("Internal Server Error", status=500)


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Quart benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Quart benchmark server on {host}:{port}")
    
    # Import and use uvicorn/hypercorn for production serving
    import uvicorn
    
    uvicorn.run(
        app,
        host=host,
        port=port,
        log_level="warning" if not DEBUG_MODE else "debug",
        access_log=DEBUG_MODE,  # Disable access logs in production for performance
        reload=DEBUG_MODE,
    )
