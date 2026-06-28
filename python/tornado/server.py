"""
Tornado Benchmark Server - Production-Grade Implementation

A high-performance benchmark server implementation using Tornado framework.
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
import signal
import sys
from typing import Any, Dict, List, Tuple

import tornado.httpserver
import tornado.ioloop
import tornado.web

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.tornado")

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("tornado").setLevel(logging.WARNING)

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


class BaseHandler(tornado.web.RequestHandler):
    """Base handler with common functionality for all endpoints."""

    def set_default_headers(self) -> None:
        """Set default response headers including security headers."""
        self.set_header("Content-Type", "text/plain")
        # Add security headers
        for header, value in SECURITY_HEADERS.items():
            self.set_header(header, value)

    def write_error(self, status_code: int, **kwargs: Any) -> None:
        """
        Override default error handling.
        
        Args:
            status_code: HTTP status code.
            **kwargs: Additional error information.
        """
        if "exc_info" in kwargs and kwargs["exc_info"]:
            exc_info = kwargs["exc_info"]
            if DEBUG_MODE:
                logger.error(
                    f"Error {status_code}: {exc_info[1]}",
                    exc_info=exc_info,
                )
            else:
                logger.warning(f"Error {status_code}: {exc_info[1]}")
        super().write_error(status_code, **kwargs)


class RootHandler(BaseHandler):
    """Handler for the root endpoint."""

    async def get(self) -> None:
        """
        Root endpoint handler.
        
        Returns:
            None: Writes empty response.
        """
        logger.debug("Root endpoint accessed")
        self.write("")


class UserHandler(BaseHandler):
    """Handler for user creation endpoint."""

    async def post(self) -> None:
        """
        Create a new user.
        
        Returns:
            None: Writes empty response.
        """
        if DEBUG_MODE:
            logger.debug("Create user endpoint accessed")
        
        # Security: Return 201 Created for POST requests
        self.set_status(201)
        self.write("")


class UserInfoHandler(BaseHandler):
    """Handler for retrieving user by ID."""

    async def get(self, id: str) -> None:
        """
        Retrieve user information by ID.
        
        Args:
            id: The user identifier from URL path.
        
        Returns:
            None: Writes user ID as response.
            
        Raises:
            ValueError: If ID is empty or invalid (security validation).
        """
        # Security: Validate input - reject empty IDs
        if not id or not id.strip():
            if DEBUG_MODE:
                logger.debug("Invalid user ID: empty")
            raise ValueError("Missing or invalid ID parameter")
        
        if DEBUG_MODE:
            logger.debug(f"User endpoint accessed with ID: {id}")
        self.write(id)


class HealthCheckHandler(BaseHandler):
    """Handler for health check endpoint."""

    async def get(self) -> None:
        """
        Health check endpoint for monitoring.
        
        Returns:
            None: Writes health status.
        """
        self.write("OK")


class ErrorHandler(BaseHandler):
    """Handler for triggering errors."""

    async def get(self) -> None:
        """
        Endpoint to trigger an error for testing error handling.
        
        Returns:
            None: This should not be reached as it raises an exception.
        """
        raise RuntimeError("Test error for error handling")


def make_app() -> tornado.web.Application:
    """
    Create and configure the Tornado application.
    
    Returns:
        tornado.web.Application: Configured application instance.
    """
    # Define handler mappings
    handlers: List[Tuple[str, type]] = [
        (r"/", RootHandler),
        (r"/user", UserHandler),
        (r"/user/(\d+)", UserInfoHandler),
        (r"/health", HealthCheckHandler),
        (r"/error", ErrorHandler),
    ]

    # Application settings
    settings = {
        "max_buffer_size": 16 * 1024 * 1024,  # 16 MB
        "gzip": False,  # Disable for benchmarking
        "log_function": logger.warning if not DEBUG_MODE else logger.info,
        "debug": DEBUG_MODE,
    }

    return tornado.web.Application(handlers=handlers, **settings)


class BenchmarkServer:
    """Tornado HTTP server wrapper for benchmarking."""

    def __init__(self, app: tornado.web.Application, port: int = 3000) -> None:
        """
        Initialize the benchmark server.
        
        Args:
            app: Tornado application instance.
            port: Port number to listen on.
        """
        self.app = app
        self.port = port
        self.server: tornado.httpserver.HTTPServer | None = None
        self.io_loop: tornado.ioloop.IOLoop | None = None

    def start(self) -> None:
        """Start the server."""
        self.server = tornado.httpserver.HTTPServer(self.app)
        self.server.bind(self.port)
        self.server.start(0)  # Number of processes
        self.io_loop = tornado.ioloop.IOLoop.current()
        
        logger.info(f"Starting Tornado benchmark server on port {self.port}")
        self.io_loop.start()

    def stop(self) -> None:
        """Stop the server gracefully."""
        if self.io_loop:
            self.io_loop.stop()
        if self.server:
            self.server.stop()


def main() -> None:
    """Main entry point for the benchmark server."""
    # Get configuration from environment or command line
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Tornado benchmark server in production mode on {host}:{port}")
    else:
        logger.info(f"Starting Tornado benchmark server on {host}:{port}")

    # Create application
    app = make_app()

    # Create and start server
    server = BenchmarkServer(app, port)

    # Handle graceful shutdown
    def signal_handler(sig: int, frame: Any) -> None:
        """Handle SIGINT and SIGTERM for graceful shutdown."""
        if DEBUG_MODE:
            logger.info("Shutting down gracefully...")
        else:
            logger.warning("Shutting down gracefully...")
        server.stop()
        sys.exit(0)

    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)

    try:
        server.start()
    except KeyboardInterrupt:
        server.stop()


if __name__ == "__main__":
    main()
