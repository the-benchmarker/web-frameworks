"""
BlackSheep Benchmark Server

A benchmark server implementation using BlackSheep framework.
Follows Python best practices including type hints, async/await, and proper error handling.
"""

from __future__ import annotations

import logging
import os
import sys
from typing import Any

from blacksheep import Request, Response
from blacksheep.server import Application
from blacksheep.server.responses import text

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.blacksheep")

# Create application with production settings
app = Application(
    debug=False,
    show_error_details=False,
    log_level=logging.INFO,
)

# Exception handler
@app.exception_handler(Exception)
async def handle_exception(request: Request, exception: Exception) -> Response:
    """
    Global exception handler for BlackSheep application.
    
    Args:
        request: BlackSheep Request object.
        exception: The exception that was raised.
    
    Returns:
        Response: Error response.
    """
    logger.error(f"Unhandled exception: {exception}", exc_info=True)
    return text("Internal Server Error", status=500)


@app.router.get("/")
async def root_handler(_: Request) -> Response:
    """
    Root endpoint handler.
    
    Args:
        _: BlackSheep Request object (unused).
    
    Returns:
        Response: Empty response for benchmarking.
    """
    logger.debug("Root endpoint accessed")
    return text("", content_type="text/plain")


@app.router.get("/user/:id")
async def get_user_handler(_: Request, id: int) -> Response:
    """
    Retrieve user information by ID.
    
    Args:
        _: BlackSheep Request object (unused).
        id: The user identifier.
    
    Returns:
        Response: The user ID as plain text.
    """
    logger.debug(f"User endpoint accessed with ID: {id}")
    return text(str(id), content_type="text/plain")


@app.router.post("/user")
async def create_user_handler(_: Request) -> Response:
    """
    Create a new user.
    
    Args:
        _: BlackSheep Request object (unused).
    
    Returns:
        Response: Empty response for benchmarking.
    """
    logger.debug("Create user endpoint accessed")
    return text("", content_type="text/plain")


@app.router.get("/health")
async def health_check_handler(_: Request) -> Response:
    """
    Health check endpoint for monitoring.
    
    Args:
        _: BlackSheep Request object (unused).
    
    Returns:
        Response: Simple health status.
    """
    return text("OK", content_type="text/plain")


if __name__ == "__main__":
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))

    logger.info(f"Starting BlackSheep benchmark server on {host}:{port}")
    
    # Run with uvicorn
    import uvicorn
    
    uvicorn.run(
        app,
        host=host,
        port=port,
        log_level="info",
        access_log=True,
    )
