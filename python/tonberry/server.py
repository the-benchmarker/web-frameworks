"""
Tonberry Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Tonberry framework.
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

from tonberry import create_app, expose, Middleware
from tonberry.content_types import TextPlain
from tonberry.request import Request
from tonberry.response import Response

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.tonberry")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("tonberry").setLevel(logging.WARNING)
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


class SecurityHeadersMiddleware:
    """Middleware to add security headers to all responses."""
    
    async def __call__(self, request: Request, call_next: Any) -> Any:
        """
        Process request and add security headers to response.
        
        Args:
            request: The Tonberry Request object.
            call_next: The next middleware/handler in the chain.
            
        Returns:
            Any: The response with security headers added.
        """
        response = await call_next(request, self)
        
        # Add security headers to all responses
        for header, value in SECURITY_HEADERS.items():
            response.headers[header] = value
        
        return response


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


class Root:
    @expose.get
    async def index(self, request: Request) -> TextPlain:
        """
        Root endpoint handler.
        
        Args:
            request: The Tonberry Request object.
            
        Returns:
            TextPlain: Empty response.
        """
        logger.debug("Root endpoint accessed")
        return TextPlain("")

    @expose.post
    async def create_user(self, request: Request) -> TextPlain:
        """
        Create a new user.
        
        Args:
            request: The Tonberry Request object.
            
        Returns:
            TextPlain: Empty response.
        """
        if DEBUG_MODE:
            logger.debug("Create user endpoint accessed")
        
        # Create response with 201 status
        response = TextPlain("")
        response.status_code = 201
        return response

    @expose.get
    async def user_info(self, request: Request, user_id: int) -> Any:
        """
        Retrieve user information by ID.
        
        Args:
            request: The Tonberry Request object.
            user_id: The user identifier from URL path.
            
        Returns:
            Any: The user ID or error response.
        """
        # Security: Validate input - reject non-positive IDs
        if user_id <= 0:
            if DEBUG_MODE:
                logger.debug(f"Invalid user ID: {user_id}")
            return TextPlain("Invalid ID parameter", status_code=400)
        
        if DEBUG_MODE:
            logger.debug(f"User endpoint accessed with ID: {user_id}")
        
        return TextPlain(str(user_id))

    @expose.get
    async def health_check(self, request: Request) -> TextPlain:
        """
        Health check endpoint for monitoring.
        
        Args:
            request: The Tonberry Request object.
            
        Returns:
            TextPlain: Simple health status.
        """
        return TextPlain("OK")

    @expose.get
    async def trigger_error(self, request: Request) -> Any:
        """
        Endpoint to trigger an error for testing error handling.
        
        Args:
            request: The Tonberry Request object.
            
        Returns:
            Any: This should not be reached as it raises an exception.
        """
        raise RuntimeError("Test error for error handling")


# =============================================================================
# ERROR HANDLERS
# =============================================================================


async def handle_exception(request: Request, exception: Exception) -> Response:
    """
    Global exception handler for Tonberry application.
    
    Args:
        request: The Tonberry Request object.
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
    for header, value in SECURITY_HEADERS.items():
        response.headers[header] = value
    return response


# =============================================================================
# APPLICATION SETUP
# =============================================================================


# Create app with middleware
app = create_app(
    root=Root,
    middlewares=[SecurityHeadersMiddleware],
    debug=DEBUG_MODE,
)

# Add exception handler
app.add_exception_handler(Exception, handle_exception)


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Tonberry benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Tonberry benchmark server on {host}:{port}")
    
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
