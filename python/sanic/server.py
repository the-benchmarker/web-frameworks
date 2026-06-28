"""
Sanic Benchmark Server

A high-performance benchmark server implementation using Sanic framework.
Follows Python best practices including async/await, type hints, and proper configuration.
"""

from __future__ import annotations

import logging
import multiprocessing
import os
from typing import Any

from sanic import Request, Sanic
from sanic.exceptions import SanicException
from sanic.response import text

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.sanic")

# Create Sanic application with optimized settings
app = Sanic("benchmark")

# Configure for production
app.config.FALLBACK_ERROR_FORMAT = "text"
app.config.RESPONSE_TIMEOUT = 300  # 5 minutes
app.config.REQUEST_MAX_SIZE = 16 * 1024 * 1024  # 16 MB
app.config.KEEPALIVE_TIMEOUT = 75
app.config.KEEPALIVE = True


@app.route("/", methods=["GET"])
async def index(request: Request) -> Any:
    """
    Root endpoint handler.
    
    Args:
        request: Sanic Request object.
    
    Returns:
        text: Empty response for benchmarking.
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
    """
    logger.debug(f"User endpoint accessed with ID: {id}")
    return text(str(id), status=200)


@app.route("/user", methods=["POST"])
async def create_user(request: Request) -> Any:
    """
    Create a new user.
    
    Args:
        request: Sanic Request object.
    
    Returns:
        text: Empty response for benchmarking.
    """
    logger.debug("Create user endpoint accessed")
    return text("", status=200)


@app.route("/health", methods=["GET"])
async def health_check(request: Request) -> Any:
    """
    Health check endpoint for monitoring.
    
    Args:
        request: Sanic Request object.
    
    Returns:
        text: Simple health status.
    """
    return text("OK", status=200)


# Global exception handler
@app.exception(SanicException, BaseException)
async def handle_exception(request: Request, exception: Exception) -> Any:
    """
    Global exception handler for Sanic application.
    
    Args:
        request: Sanic Request object.
        exception: The exception that was raised.
    
    Returns:
        text: Error response.
    """
    logger.error(f"Unhandled exception: {exception}", exc_info=True)
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
    return text("Not Found", status=404)


if __name__ == "__main__":
    import sys

    # Get configuration from environment or use defaults
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    workers = int(os.getenv("WORKERS", multiprocessing.cpu_count()))

    logger.info(f"Starting Sanic benchmark server on {host}:{port} with {workers} workers")
    
    app.run(
        host=host,
        port=port,
        workers=workers,
        debug=False,
        access_log=True,
        auto_reload=False,
    )
