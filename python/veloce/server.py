"""
Veloce Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Veloce framework.
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

from veloce import PlainTextResponse, Veloce
from veloce.request import Request

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.veloce")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("veloce").setLevel(logging.WARNING)
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
    
    def __init__(self, content: str = "", status_code: int = 200, headers: Dict[str, str] | None = None) -> None:
        """
        Initialize the response with security headers.
        
        Args:
            content: Response content.
            status_code: HTTP status code.
            headers: Additional headers to add.
        """
        super().__init__(content=content, status_code=status_code)
        # Add security headers
        for header, value in SECURITY_HEADERS.items():
            self.headers[header] = value
        # Add content type if not already set
        if "Content-Type" not in self.headers:
            self.headers["Content-Type"] = "text/plain"
        # Add any additional headers
        if headers:
            for header, value in headers.items():
                self.headers[header] = value


# =============================================================================
# APPLICATION SETUP
# =============================================================================


app = Veloce(debug=DEBUG_MODE)


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


@app.get("/")
async def index(request: Request) -> SecurityHeadersResponse:
    """
    Root endpoint handler.
    
    Args:
        request: The Veloce Request object.
        
    Returns:
        SecurityHeadersResponse: Empty response with security headers.
    """
    logger.debug("Root endpoint accessed")
    return SecurityHeadersResponse(content="")


@app.get("/user/{id}")
async def get_user(request: Request, id: int) -> Any:
    """
    Retrieve user information by ID.
    
    Args:
        request: The Veloce Request object.
        id: The user identifier from URL path.
        
    Returns:
        Any: Response containing the user ID with security headers.
        
    Raises:
        SecurityHeadersResponse: If ID is invalid (security validation).
    """
    # Security: Validate input - reject non-positive IDs
    if id <= 0:
        if DEBUG_MODE:
            logger.debug(f"Invalid user ID: {id}")
        return SecurityHeadersResponse(content="Invalid ID parameter", status_code=400)
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {id}")
    
    return SecurityHeadersResponse(content=str(id))


@app.post("/user")
async def create_user(request: Request) -> SecurityHeadersResponse:
    """
    Create a new user.
    
    Args:
        request: The Veloce Request object.
        
    Returns:
        SecurityHeadersResponse: Empty response with 201 status and security headers.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    
    # Security: Return 201 Created for POST requests
    return SecurityHeadersResponse(content="", status_code=201)


@app.get("/health")
async def health_check(request: Request) -> SecurityHeadersResponse:
    """
    Health check endpoint for monitoring.
    
    Args:
        request: The Veloce Request object.
        
    Returns:
        SecurityHeadersResponse: Simple health status with security headers.
    """
    return SecurityHeadersResponse(content="OK")


@app.get("/error")
async def trigger_error(request: Request) -> Any:
    """
    Endpoint to trigger an error for testing error handling.
    
    Args:
        request: The Veloce Request object.
        
    Returns:
        Any: This should not be reached as it raises an exception.
    """
    raise RuntimeError("Test error for error handling")


# =============================================================================
# ERROR HANDLERS
# =============================================================================


@app.exception_handler()
async def handle_exception(request: Request, exception: Exception) -> SecurityHeadersResponse:
    """
    Global exception handler for Veloce application.
    
    Args:
        request: The Veloce Request object.
        exception: The exception that was raised.
        
    Returns:
        SecurityHeadersResponse: Error response with security headers.
    """
    if DEBUG_MODE:
        logger.exception(f"Unhandled exception: {exception}")
    else:
        logger.warning(f"Unhandled exception: {type(exception).__name__}")
    
    # Don't expose internal error details in production
    return SecurityHeadersResponse(content="Internal Server Error", status_code=500)


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Veloce benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Veloce benchmark server on {host}:{port}")
    
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
