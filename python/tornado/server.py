"""
Tornado Benchmark Server

A high-performance benchmark server implementation using Tornado framework.
Follows Python best practices including type hints, proper error handling, and async networking.
"""

from __future__ import annotations

import logging
import os
import signal
import sys
from typing import Any, List, Tuple

import tornado.httpserver
import tornado.ioloop
import tornado.web

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.tornado")


class BaseHandler(tornado.web.RequestHandler):
    """Base handler with common functionality for all endpoints."""

    def set_default_headers(self) -> None:
        """Set default response headers."""
        self.set_header("Content-Type", "text/plain")

    def write_error(self, status_code: int, **kwargs: Any) -> None:
        """
        Override default error handling.
        
        Args:
            status_code: HTTP status code.
            **kwargs: Additional error information.
        """
        if "exc_info" in kwargs and kwargs["exc_info"]:
            exc_info = kwargs["exc_info"]
            logger.error(
                f"Error {status_code}: {exc_info[1]}",
                exc_info=exc_info,
            )
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
        logger.debug("Create user endpoint accessed")
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
        """
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
    ]

    # Application settings
    settings = {
        "max_buffer_size": 16 * 1024 * 1024,  # 16 MB
        "gzip": False,  # Disable for benchmarking
        "log_function": logger.info,
        "debug": False,
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
    # Get port from environment or command line
    port = int(os.getenv("PORT", sys.argv[1] if len(sys.argv) > 1 else "3000"))

    # Create application
    app = make_app()

    # Create and start server
    server = BenchmarkServer(app, port)

    # Handle graceful shutdown
    def signal_handler(sig: int, frame: Any) -> None:
        """Handle SIGINT and SIGTERM for graceful shutdown."""
        logger.info("Shutting down gracefully...")
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
