"""
Starlette Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Starlette framework.
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
from typing import Any, Callable, Dict

from starlette.applications import Starlette
from starlette.exceptions import HTTPException
from starlette.middleware import Middleware
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.requests import Request
from starlette.responses import PlainTextResponse
from starlette.routing import Route

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.starlette")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("starlette").setLevel(logging.WARNING)
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


# Custom middleware for security headers and logging
class SecurityHeadersMiddleware(BaseHTTPMiddleware):
    """Middleware to add security headers to all responses."""
    
    async def dispatch(self, request: Request, call_next: Callable[..., Any]) -> Any:
        """
        Dispatch request to the next middleware or route handler.
        
        Args:
            request: The incoming request.
            call_next: The next middleware or route handler.
            
        Returns:
            Response from the next handler with security headers.
        """
        response = await call_next(request)
        
        # Add security headers to all responses
        for header, value in SECURITY_HEADERS.items():
            response.headers[header] = value
        
        return response


# Custom middleware for logging and error handling
class LoggingMiddleware(BaseHTTPMiddleware):
    """Middleware for request logging and error handling."""

    async def dispatch(self, request: Request, call_next: Callable[..., Any]) -> Any:
        """
        Dispatch request to the next middleware or route handler.
        
        Args:
            request: The incoming request.
            call_next: The next middleware or route handler.
        
        Returns:
            Response from the next handler.
        """
        logger.debug(f"{request.method} {request.url.path}")
        try:
            return await call_next(request)
        except Exception as error:
            if DEBUG_MODE:
                logger.exception(f"Error handling request: {error}")
            else:
                logger.warning(f"Error handling request: {type(error).__name__}")
            return PlainTextResponse(
                content="Internal Server Error",
                status_code=500,
            )


# Route handlers
async def root_handler(request: Request) -> PlainTextResponse:
    """
    Root endpoint handler.
    
    Args:
        request: Starlette Request object.
    
    Returns:
        PlainTextResponse: Empty response for benchmarking.
    """
    logger.debug("Root endpoint accessed")
    return PlainTextResponse(content="")


async def get_user_handler(request: Request) -> PlainTextResponse:
    """
    Retrieve user information by ID.
    
    Args:
        request: Starlette Request object.
    
    Returns:
        PlainTextResponse: The user ID as plain text.
        
    Raises:
        HTTPException: If ID is empty or invalid (security validation).
    """
    user_id = request.path_params["user_id"]
    
    # Security: Validate input - reject empty IDs
    if not user_id or not user_id.strip():
        if DEBUG_MODE:
            logger.debug("Invalid user ID: empty")
        raise HTTPException(status_code=400, detail="Missing or invalid ID parameter")
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {user_id}")
    
    return PlainTextResponse(content=user_id)


async def create_user_handler(request: Request) -> PlainTextResponse:
    """
    Create a new user.
    
    Args:
        request: Starlette Request object.
    
    Returns:
        PlainTextResponse: Empty response with 201 status.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    
    # Security: Return 201 Created for POST requests
    return PlainTextResponse(content="", status_code=201)


async def health_check_handler(request: Request) -> PlainTextResponse:
    """
    Health check endpoint for monitoring.
    
    Args:
        request: Starlette Request object.
    
    Returns:
        PlainTextResponse: Simple health status.
    """
    return PlainTextResponse(content="OK")


async def error_handler(request: Request) -> Any:
    """
    Endpoint to trigger an error for testing error handling.
    
    Args:
        request: Starlette Request object.
    
    Returns:
        Any: This should not be reached as it raises an exception.
    """
    raise RuntimeError("Test error for error handling")


# Exception handler for HTTP exceptions
async def http_exception_handler(request: Request, exc: HTTPException) -> PlainTextResponse:
    """
    Handle HTTP exceptions.
    
    Args:
        request: Starlette Request object.
        exc: The HTTPException that was raised.
    
    Returns:
        PlainTextResponse: Error response.
    """
    logger.error(f"HTTP Error: {exc.status_code} - {exc.detail}")
    return PlainTextResponse(content=exc.detail or "Error", status_code=exc.status_code)


def create_app() -> Starlette:
    """
    Create and configure the Starlette application.
    
    Returns:
        Starlette: Configured application instance.
    """
    # Define routes
    routes = [
        Route("/", root_handler, methods=["GET"]),
        Route("/user/{user_id}", get_user_handler, methods=["GET"]),
        Route("/user", create_user_handler, methods=["POST"]),
        Route("/health", health_check_handler, methods=["GET"]),
        Route("/error", error_handler, methods=["GET"]),
    ]

    # Configure middleware
    middleware = [
        Middleware(LoggingMiddleware),
        Middleware(SecurityHeadersMiddleware),
    ]

    # Create and configure application
    app = Starlette(
        routes=routes,
        middleware=middleware,
        debug=DEBUG_MODE,
    )

    # Configure exception handlers
    app.add_exception_handler(HTTPException, http_exception_handler)
    app.add_exception_handler(Exception, lambda r, e: PlainTextResponse(
        content="Internal Server Error",
        status_code=500,
    ))

    return app


app = create_app()


if __name__ == "__main__":
    import uvicorn

    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Starlette benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Starlette benchmark server on {host}:{port}")
    
    # Run with uvicorn
    uvicorn.run(
        app,
        host=host,
        port=port,
        log_level="warning" if not DEBUG_MODE else "debug",
        access_log=DEBUG_MODE,  # Disable access logs in production for performance
        reload=DEBUG_MODE,
    )
