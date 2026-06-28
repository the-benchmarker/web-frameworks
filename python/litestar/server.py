"""
Litestar Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Litestar framework.
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
from typing import Any, Dict, List

from litestar import Litestar, MediaType, Request, Response, get, post
from litestar.exceptions import HTTPException
from litestar.middleware import Middleware
from litestar.status_codes import (
    HTTP_200_OK,
    HTTP_201_CREATED,
    HTTP_400_BAD_REQUEST,
    HTTP_404_NOT_FOUND,
    HTTP_500_INTERNAL_SERVER_ERROR,
)

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.litestar")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("litestar").setLevel(logging.WARNING)
    logging.getLogger("uvicorn").setLevel(logging.WARNING)

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


class SecurityHeadersMiddleware:
    """
    Middleware to add security headers to all responses.
    
    Security best practices:
    - X-Content-Type-Options: nosniff prevents MIME type sniffing
    - X-Frame-Options: DENY prevents clickjacking
    - X-XSS-Protection: enables XSS protection in browsers
    - Content-Security-Policy: restricts resource loading
    - Referrer-Policy: controls referrer information
    - Cache-Control: prevents caching of sensitive data
    """

    async def __call__(self, request: Request, call_next: Any) -> Response:
        """
        Process request and add security headers to response.
        
        Args:
            request: The Litestar Request object.
            call_next: The next middleware/handler in the chain.
            
        Returns:
            Response: The response with security headers added.
        """
        response = await call_next()
        
        # Add security headers to all responses
        for header, value in SECURITY_HEADERS.items():
            response.headers[header] = value
        
        return response


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


@get("/")
async def index() -> Response[str]:
    """
    Root endpoint handler.
    
    Returns:
        Response[str]: Empty response with security headers.
    """
    logger.debug("Root endpoint accessed")
    return Response(content="", media_type=MediaType.TEXT)


@get("/user/{id:int}")
async def get_user(id: int) -> Response[str]:
    """
    Retrieve user information by ID.
    
    Args:
        id: The user identifier from URL path.
        
    Returns:
        Response[str]: Response containing the user ID.
        
    Raises:
        HTTPException: If ID is invalid (security validation).
    """
    # Security: Validate input - reject non-positive IDs
    if id <= 0:
        if DEBUG_MODE:
            logger.debug(f"Invalid user ID: {id}")
        raise HTTPException(
            detail="Invalid ID parameter",
            status_code=HTTP_400_BAD_REQUEST,
        )
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {id}")
    
    return Response(content=str(id), media_type=MediaType.TEXT)


@post("/user")
async def create_user() -> Response[str]:
    """
    Create a new user.
    
    Returns:
        Response[str]: Empty response with 201 status and security headers.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    
    # Security: Return 201 Created for POST requests
    return Response(content="", media_type=MediaType.TEXT, status_code=HTTP_201_CREATED)


@get("/health")
async def health_check() -> Response[str]:
    """
    Health check endpoint for monitoring.
    
    Returns:
        Response[str]: Simple health status response.
    """
    return Response(content="OK", media_type=MediaType.TEXT)


@get("/error")
async def trigger_error() -> Response[str]:
    """
    Endpoint to trigger an error for testing error handling.
    
    Returns:
        Response[str]: This should not be reached as it raises an exception.
    """
    raise RuntimeError("Test error for error handling")


# =============================================================================
# ERROR HANDLERS
# =============================================================================


async def handle_exception(request: Request, exc: Exception) -> Response[str]:
    """
    Global exception handler for Litestar application.
    
    Args:
        request: The Litestar Request object.
        exc: The exception that was raised.
        
    Returns:
        Response[str]: Error response with security headers.
    """
    if DEBUG_MODE:
        logger.exception(f"Unhandled exception: {exc}")
    else:
        logger.warning(f"Unhandled exception: {type(exc).__name__}")
    
    # Don't expose internal error details in production
    error_message = "Internal Server Error" if not DEBUG_MODE else str(exc)
    
    return Response(
        content=error_message,
        media_type=MediaType.TEXT,
        status_code=HTTP_500_INTERNAL_SERVER_ERROR,
    )


def handle_404(request: Request, exc: HTTPException) -> Response[str]:
    """
    Handle 404 Not Found errors.
    
    Args:
        request: The Litestar Request object.
        exc: The HTTPException.
        
    Returns:
        Response[str]: 404 response with security headers.
    """
    if DEBUG_MODE:
        logger.debug(f"404 Not Found: {request.url.path}")
    
    return Response(
        content="Not Found",
        media_type=MediaType.TEXT,
        status_code=HTTP_404_NOT_FOUND,
    )


# =============================================================================
# APPLICATION SETUP
# =============================================================================


# Configure application with production settings
app = Litestar(
    route_handlers=[
        index,
        get_user,
        create_user,
        health_check,
        trigger_error,
    ],
    middleware=[
        Middleware(SecurityHeadersMiddleware),
    ],
    exception_handlers={
        RuntimeError: handle_exception,
        Exception: handle_exception,
    },
    debug=DEBUG_MODE,
    openapi_config=None,  # Disable OpenAPI docs for production
    # Security: Disable potentially dangerous features
    allowed_hosts=["*"],  # Configure proper host validation in production
    cors_config=None,  # Disable CORS for benchmarking (configure properly in real apps)
)


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Litestar benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Litestar benchmark server on {host}:{port}")
    
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
