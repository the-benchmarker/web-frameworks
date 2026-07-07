"""
Panther Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Panther framework.
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

from panther import Panther
from panther.app import API
from panther.response import PlainTextResponse

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.panther")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("panther").setLevel(logging.WARNING)
    logging.getLogger("uvicorn").setLevel(logging.WARNING)

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


class SecurityHeadersResponse(PlainTextResponse):
    """Response class with security headers pre-configured."""
    
    def __init__(self, content: str, status_code: int = 200, headers: Dict[str, str] | None = None) -> None:
        """
        Initialize the response with security headers.
        
        Args:
            content: Response content.
            status_code: HTTP status code.
            headers: Additional headers to add.
        """
        super().__init__(content)
        self.status_code = status_code
        # Add security headers
        for header, value in SECURITY_HEADERS.items():
            self.headers[header] = value
        # Add content type
        self.headers["Content-Type"] = "text/plain"
        # Add any additional headers
        if headers:
            for header, value in headers.items():
                self.headers[header] = value


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


@API(methods=["GET"])
async def index():
    """
    Root endpoint handler.
    
    Returns:
        SecurityHeadersResponse: Empty response with security headers.
    """
    logger.debug("Root endpoint accessed")
    return SecurityHeadersResponse("")


@API(methods=["GET"])
async def get_user(id: str):
    """
    Retrieve user information by ID.
    
    Args:
        id: The user identifier from URL path.
        
    Returns:
        SecurityHeadersResponse: Response containing the user ID with security headers.
        
    Raises:
        ValueError: If ID is empty or invalid (security validation).
    """
    # Security: Validate input - reject empty IDs
    if not id or not id.strip():
        if DEBUG_MODE:
            logger.debug("Invalid user ID: empty")
        raise ValueError("Missing or invalid ID parameter")
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {id}")
    
    return SecurityHeadersResponse(id)


@API(methods=["POST"])
async def create_user() -> str:
    """
    Create a new user.
    
    Returns:
        SecurityHeadersResponse: Empty response with 201 status and security headers.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    
    # Security: Return 201 Created for POST requests
    return SecurityHeadersResponse("", status_code=201)


@API(methods=["GET"])
async def health_check():
    """
    Health check endpoint for monitoring.
    
    Returns:
        SecurityHeadersResponse: Simple health status response.
    """
    return SecurityHeadersResponse("OK")


@API(methods=["GET"])
async def trigger_error():
    """
    Endpoint to trigger an error for testing error handling.
    
    Returns:
        SecurityHeadersResponse: This should not be reached as it raises an exception.
    """
    raise RuntimeError("Test error for error handling")


# =============================================================================
# ERROR HANDLERS
# =============================================================================


async def error_handler(request: Any, exc: Exception) -> SecurityHeadersResponse:
    """
    Global exception handler for Panther application.
    
    Args:
        request: The request object.
        exc: The exception that was raised.
        
    Returns:
        SecurityHeadersResponse: Error response with security headers.
    """
    if DEBUG_MODE:
        logger.exception(f"Unhandled exception: {exc}")
    else:
        logger.warning(f"Unhandled exception: {type(exc).__name__}")
    
    return SecurityHeadersResponse("Internal Server Error", status_code=500)


# =============================================================================
# APPLICATION SETUP
# =============================================================================


urls = {
    "/": index,
    "/user/<id>": get_user,
    "/user": create_user,
    "/health": health_check,
    "/error": trigger_error,
}

app = Panther(__name__, configs=__name__, urls=urls)

# Add error handler
app.error_handler = error_handler


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Panther benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Panther benchmark server on {host}:{port}")
    
    # Import and use uvicorn for production serving
    import uvicorn
    
    uvicorn.run(
        app,
        host=host,
        port=port,
        log_level="warning" if not DEBUG_MODE else "debug",
        access_log=DEBUG_MODE,  # Disable access logs in production for performance
        reload=DEBUG_MODE,
    )
