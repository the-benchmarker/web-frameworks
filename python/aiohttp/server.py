"""
Aiohttp Benchmark Server

A high-performance benchmark server implementation using aiohttp framework.
Follows Python best practices including async/await, type hints, and proper error handling.
"""

from __future__ import annotations

import logging
import sys
from typing import cast

from aiohttp import web

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.aiohttp")

# Define routes using RouteTableDef
routes = web.RouteTableDef()


@routes.get("/", name="index")
async def index(request: web.Request) -> web.Response:
    """
    Root endpoint handler.
    
    Args:
        request: The aiohttp request object.
    
    Returns:
        web.Response: Empty response for benchmarking.
    """
    logger.debug("Root endpoint accessed")
    return web.Response(text="", content_type="text/plain")


@routes.post("/user", name="create_user")
async def create_user(request: web.Request) -> web.Response:
    """
    Create a new user.
    
    Args:
        request: The aiohttp request object.
    
    Returns:
        web.Response: Empty response for benchmarking.
    """
    logger.debug("Create user endpoint accessed")
    return web.Response(text="", content_type="text/plain")


@routes.get("/user/{id}", name="get_user")
async def get_user(request: web.Request) -> web.Response:
    """
    Retrieve user information by ID.
    
    Args:
        request: The aiohttp request object.
    
    Returns:
        web.Response: The user ID as plain text.
    """
    user_id = cast(str, request.match_info["id"])
    logger.debug(f"User endpoint accessed with ID: {user_id}")
    return web.Response(text=user_id, content_type="text/plain")


# Error handling middleware
@web.middleware
async def error_middleware(
    request: web.Request, handler: web.Handler
) -> web.Response:
    """
    Middleware for error handling.
    
    Args:
        request: The aiohttp request object.
        handler: The request handler.
    
    Returns:
        web.Response: Response from handler or error response.
    """
    try:
        return await handler(request)
    except Exception as error:
        logger.error(f"Unhandled exception: {error}", exc_info=True)
        return web.Response(
            text="Internal Server Error",
            status=500,
            content_type="text/plain",
        )


# Health check endpoint
@routes.get("/health", name="health_check")
async def health_check(request: web.Request) -> web.Response:
    """
    Health check endpoint for monitoring.
    
    Args:
        request: The aiohttp request object.
    
    Returns:
        web.Response: Simple health status.
    """
    return web.Response(text="OK", content_type="text/plain")


async def create_app() -> web.Application:
    """
    Create and configure the aiohttp application.
    
    Returns:
        web.Application: Configured aiohttp application.
    """
    app = web.Application(
        middlewares=[error_middleware],
        client_max_size=16 * 1024 * 1024,  # 16 MB
    )
    app.add_routes(routes)
    return app


if __name__ == "__main__":
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 3000
    web.run_app(create_app(), port=port)
