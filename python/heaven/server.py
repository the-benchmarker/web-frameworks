"""
Heaven Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Heaven framework.
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

from heaven import Router

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.heaven")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("heaven").setLevel(logging.WARNING)
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


def add_security_headers(res: Any) -> None:
    """
    Add security headers to a response.
    
    Args:
        res: The response object to add headers to.
    """
    for header, value in SECURITY_HEADERS.items():
        res.headers[header] = value


# =============================================================================
# REQUEST HANDLERS
# =============================================================================


async def index_handler(req: Any, res: Any, ctx: Any) -> None:
    """
    Root endpoint handler.
    
    Args:
        req: The request object.
        res: The response object.
        ctx: Context object.
    """
    logger.debug("Root endpoint accessed")
    res.body = ""
    res.status = 200
    add_security_headers(res)


async def user_info_handler(req: Any, res: Any, ctx: Any) -> None:
    """
    Retrieve user information by ID.
    
    Args:
        req: The request object.
        res: The response object.
        ctx: Context object.
        
    Raises:
        ValueError: If ID is empty or invalid (security validation).
    """
    id_ = req.params.get('id', '')
    
    # Security: Validate input - reject empty IDs
    if not id_ or not id_.strip():
        if DEBUG_MODE:
            logger.debug("Invalid user ID: empty")
        raise ValueError("Missing or invalid ID parameter")
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {id_}")
    
    res.body = str(id_)
    res.status = 200
    add_security_headers(res)


async def create_user_handler(req: Any, res: Any, ctx: Any) -> None:
    """
    Create a new user.
    
    Args:
        req: The request object.
        res: The response object.
        ctx: Context object.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    
    # Security: Return 201 Created for POST requests
    res.body = ""
    res.status = 201
    add_security_headers(res)


async def health_check_handler(req: Any, res: Any, ctx: Any) -> None:
    """
    Health check endpoint for monitoring.
    
    Args:
        req: The request object.
        res: The response object.
        ctx: Context object.
    """
    res.body = "OK"
    res.status = 200
    add_security_headers(res)


# =============================================================================
# ERROR HANDLERS
# =============================================================================


async def error_handler(req: Any, res: Any, ctx: Any) -> None:
    """
    Error endpoint to trigger exceptions for testing.
    
    Args:
        req: The request object.
        res: The response object.
        ctx: Context object.
    """
    raise RuntimeError("Test error for error handling")


# Global exception handler for Heaven
async def exception_handler(req: Any, res: Any, ctx: Any, exception: Exception) -> None:
    """
    Global exception handler for Heaven application.
    
    Args:
        req: The request object.
        res: The response object.
        ctx: Context object.
        exception: The exception that was raised.
    """
    if DEBUG_MODE:
        logger.exception(f"Unhandled exception: {exception}")
    else:
        logger.warning(f"Unhandled exception: {type(exception).__name__}")
    
    res.body = "Internal Server Error"
    res.status = 500
    add_security_headers(res)


# =============================================================================
# APPLICATION SETUP
# =============================================================================


# Create Heaven router application
app = Router()

# Add routes with production-grade handlers
app.GET("/", index_handler)
app.GET("/user/:id", user_info_handler)
app.POST("/user", create_user_handler)
app.GET("/health", health_check_handler)

# Error handler route
app.GET("/error", error_handler)

# Add error handling middleware
@app.middleware
async def error_middleware(req: Any, res: Any, ctx: Any, next_handler: Any) -> None:
    """
    Error handling middleware for Heaven.
    
    Args:
        req: The request object.
        res: The response object.
        ctx: Context object.
        next_handler: The next middleware/handler in the chain.
    """
    try:
        await next_handler(req, res, ctx)
    except Exception as e:
        await exception_handler(req, res, ctx, e)


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Heaven benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Heaven benchmark server on {host}:{port}")
    
    # Import and use uvicorn for production serving
    import uvicorn
    
    # Heaven router is ASGI compatible, run directly with uvicorn
    uvicorn.run(
        app,
        host=host,
        port=port,
        log_level="warning" if not DEBUG_MODE else "debug",
        access_log=DEBUG_MODE,  # Disable access logs in production for performance
        reload=DEBUG_MODE,
    )
        
    # Run with uvicorn for production
    uvicorn.run(
        asgi_app,
        host=host,
        port=port,
        log_level="warning" if not DEBUG_MODE else "debug",
        access_log=DEBUG_MODE,  # Disable access logs in production for performance
        reload=DEBUG_MODE,
    )
