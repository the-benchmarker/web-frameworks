"""
Starlette Benchmark Server

A benchmark server implementation using Starlette framework.
Follows Python best practices including async/await, type hints, and proper error handling.
"""

from __future__ import annotations

import logging
import os
import sys
from typing import Any, Callable

from starlette.applications import Starlette
from starlette.exceptions import HTTPException
from starlette.middleware import Middleware
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.requests import Request
from starlette.responses import PlainTextResponse
from starlette.routing import Route

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.starlette")


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
            logger.error(f"Error handling request: {error}", exc_info=True)
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
    """
    user_id = request.path_params["user_id"]
    logger.debug(f"User endpoint accessed with ID: {user_id}")
    return PlainTextResponse(content=user_id)


async def create_user_handler(request: Request) -> PlainTextResponse:
    """
    Create a new user.
    
    Args:
        request: Starlette Request object.
    
    Returns:
        PlainTextResponse: Empty response for benchmarking.
    """
    logger.debug("Create user endpoint accessed")
    return PlainTextResponse(content="")


async def health_check_handler(request: Request) -> PlainTextResponse:
    """
    Health check endpoint for monitoring.
    
    Args:
        request: Starlette Request object.
    
    Returns:
        PlainTextResponse: Simple health status.
    """
    return PlainTextResponse(content="OK")


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
    ]

    # Configure middleware
    middleware = [
        Middleware(LoggingMiddleware),
    ]

    # Create and configure application
    app = Starlette(
        routes=routes,
        middleware=middleware,
        debug=False,
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

    logger.info(f"Starting Starlette benchmark server on {host}:{port}")
    
    # Run with uvicorn
    uvicorn.run(
        app,
        host=host,
        port=port,
        log_level="info",
        access_log=True,
    )
