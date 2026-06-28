#!/usr/bin/env python3
"""
CherryPy Benchmark Server

A benchmark server implementation using CherryPy framework.
Follows Python best practices including type hints, proper error handling, logging,
and class-based organization.
"""

from __future__ import annotations

import logging
import os
import sys

import cherrypy
from cherrypy import expose, tools

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.cherrypy")

IS_STANDALONE = __name__ == "__main__"


class HealthCheckController:
    """Health check endpoint controller."""

    @expose
    def index(self) -> str:
        """
        Health check endpoint for monitoring.
        
        Returns:
            str: Health status.
        """
        return "OK"


class UserController:
    """User-related endpoint controller."""

    @expose
    def GET(self, user_id: str) -> str:
        """
        Retrieve user information by ID.
        
        Args:
            user_id: The user identifier from URL path.
        
        Returns:
            str: The user ID as plain text.
        """
        logger.debug(f"User endpoint accessed with ID: {user_id}")
        return user_id

    @expose
    def POST(self) -> str:
        """
        Create a new user.
        
        Returns:
            str: Empty response for benchmarking.
        """
        logger.debug("Create user endpoint accessed")
        return ""


class RootController:
    """Root endpoint controller."""

    @expose
    def index(self) -> str:
        """
        Root endpoint handler.
        
        Returns:
            str: Empty response for benchmarking.
        """
        logger.debug("Root endpoint accessed")
        return ""


# Configure the application
class BenchmarkApplication:
    """Main application configuration."""

    def __init__(self) -> None:
        """Initialize the benchmark application."""
        self.root = RootController()
        self.user = UserController()
        self.health = HealthCheckController()

    def mount(self) -> cherrypy.Application:
        """
        Mount the application with proper configuration.
        
        Returns:
            cherrypy.Application: Mounted CherryPy application.
        """
        # Configuration for user endpoints (method dispatcher)
        user_config = {
            "request.dispatch": cherrypy.dispatch.MethodDispatcher(),
        }

        # Global configuration
        global_config = {
            "environment": "production" if IS_STANDALONE else "embedded",
            "log.screen": True,
            "log.access_file": "",
            "log.error_file": "",
            "server.socket_host": "0.0.0.0",
            "server.socket_port": int(os.getenv("PORT", "3000")),
            "server.thread_pool": 30,  # Adjust based on workload
            "server.max_request_body_size": 16 * 1024 * 1024,  # 16 MB
            "response.timeout": 300,  # 5 minutes timeout
        }

        if not IS_STANDALONE:
            # When running on top of another WSGI server
            cherrypy.server.unsubscribe()
            cherrypy.engine.start()

        # Mount controllers
        root = self.root
        root.user = self.user
        root.health = self.health

        # Apply configuration
        cherrypy.config.update(global_config)

        # Mount the application
        app = cherrypy.tree.mount(root, "", {"/user": user_config, "/health": {}})
        
        return app


# Create application instance
app = BenchmarkApplication().mount()

# Error handling tool
@cherrypy.tool("before_error_response")
def handle_error() -> None:
    """Custom error handler for CherryPy."""
    if cherrypy.response.status.startswith("500"):
        logger.error(
            f"Server error: {cherrypy.response.status} - {cherrypy.request.path_info}",
            exc_info=cherrypy.request.error_traceback,
        )


handle_error.subscribe()

if not IS_STANDALONE:
    # Expose app for WSGI
    application = app

# Run in standalone mode if executed directly
if IS_STANDALONE:
    # Get port from environment or command line
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 3000
    
    cherrypy.config.update({
        "server.socket_port": port,
    })
    
    logger.info(f"Starting CherryPy benchmark server on port {port}")
    cherrypy.quickstart(app)
