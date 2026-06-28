"""
Flask Benchmark Server

A benchmark server implementation using Flask framework.
Follows Python best practices including type hints, proper error handling, and logging.
"""

from __future__ import annotations

import logging
from typing import Union

from flask import Flask, Response

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.flask")

# Create Flask application
app = Flask(__name__)

# Disable features not needed for benchmarking
app.config["JSONIFY_PRETTYPRINT_REGULAR"] = False
app.config["MAX_CONTENT_LENGTH"] = 16 * 1024 * 1024  # 16 MB


@app.route("/", methods=["GET"])
def index() -> Response:
    """
    Root endpoint handler.
    
    Returns:
        Response: Empty response for benchmarking.
    """
    logger.debug("Root endpoint accessed")
    return Response(response="", status=200, mimetype="text/plain")


@app.route("/user/<int:id>", methods=["GET"])
def user_info(id: int) -> Union[Response, str]:
    """
    Retrieve user information by ID.
    
    Args:
        id: The user identifier.
    
    Returns:
        Response or str: The user ID as plain text.
    """
    logger.debug(f"User endpoint accessed with ID: {id}")
    return str(id)


@app.route("/user", methods=["POST"])
def create_user() -> Response:
    """
    Create a new user.
    
    Returns:
        Response: Empty response for benchmarking.
    """
    logger.debug("Create user endpoint accessed")
    return Response(response="", status=200, mimetype="text/plain")


@app.errorhandler(Exception)
def handle_exception(error: Exception) -> Response:
    """
    Global exception handler.
    
    Args:
        error: The exception that was raised.
    
    Returns:
        Response: Error response.
    """
    logger.error(f"Unhandled exception: {error}", exc_info=True)
    return Response(
        response="Internal Server Error",
        status=500,
        mimetype="text/plain",
    )


# Health check endpoint
@app.route("/health", methods=["GET"])
def health_check() -> Response:
    """
    Health check endpoint for monitoring.
    
    Returns:
        Response: Simple health status.
    """
    return Response(response="OK", status=200, mimetype="text/plain")


if __name__ == "__main__":
    import os

    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", 3000))
    
    app.run(host=host, port=port, debug=False, threaded=True)
