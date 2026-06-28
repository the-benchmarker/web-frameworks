"""
FastAPI Benchmark Server

A high-performance benchmark server implementation using FastAPI framework.
Follows Python best practices including type hints, async/await, and proper error handling.
"""

from __future__ import annotations

import logging
from typing import Annotated

from fastapi import FastAPI, Path
from fastapi.responses import PlainTextResponse

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.fastapi")

# Create FastAPI application
app = FastAPI(
    title="FastAPI Benchmark Server",
    description="Benchmark server for web framework comparison",
    version="1.0.0",
    docs_url=None,  # Disable docs for benchmarking
    redoc_url=None,
)


@app.get("/", response_class=PlainTextResponse, summary="Root endpoint")
async def index() -> PlainTextResponse:
    """
    Root endpoint handler.
    
    Returns:
        PlainTextResponse: Empty response for benchmarking.
    """
    logger.debug("Root endpoint accessed")
    return PlainTextResponse(content="")


@app.get(
    "/user/{id}",
    response_class=PlainTextResponse,
    summary="Get user by ID",
)
async def get_user(
    id: Annotated[int, Path(ge=0, le=2_147_483_647, description="User ID")],
) -> PlainTextResponse:
    """
    Retrieve user information by ID.
    
    Args:
        id: The user identifier.
    
    Returns:
        PlainTextResponse: The user ID as plain text.
    
    Raises:
        HTTPException: If ID validation fails (handled by FastAPI).
    """
    logger.debug(f"User endpoint accessed with ID: {id}")
    return PlainTextResponse(content=str(id))


@app.post("/user", response_class=PlainTextResponse, summary="Create user")
async def create_user() -> PlainTextResponse:
    """
    Create a new user.
    
    Returns:
        PlainTextResponse: Empty response for benchmarking.
    """
    logger.debug("Create user endpoint accessed")
    return PlainTextResponse(content="")


# Health check endpoint (optional, for monitoring)
@app.get("/health", response_class=PlainTextResponse, include_in_schema=False)
async def health_check() -> PlainTextResponse:
    """
    Health check endpoint for monitoring.
    
    Returns:
        PlainTextResponse: Simple health status.
    """
    return PlainTextResponse(content="OK")


if __name__ == "__main__":
    import uvicorn

    uvicorn.run(
        app,
        host="0.0.0.0",
        port=3000,
        log_level="info",
        access_log=True,
    )
