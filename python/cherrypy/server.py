#!/usr/bin/env python3
"""
CherryPy Benchmark Server - Production-Grade Implementation

A benchmark server implementation using CherryPy framework.
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

import cherrypy
from cherrypy import expose, tools

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure minimal logging for production
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,
    format="%(asctime)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.cherrypy")

# Suppress CherryPy access logs in production for performance
if not DEBUG_MODE:
    cherrypy.log.access_log.propagate = False
    cherrypy.log.error_log.propagate = False

IS_STANDALONE = __name__ == "__main__"


# =============================================================================
# SECURITY HEADERS TOOL
# =============================================================================


class SecurityHeadersTool(tools.Tool):
    """
    CherryPy tool to add security headers to all responses.
    
    Security best practices:
    - X-Content-Type-Options: nosniff prevents MIME type sniffing
    - X-Frame-Options: DENY prevents clickjacking
    - X-XSS-Protection: enables XSS protection in browsers
    - Content-Security-Policy: restricts resource loading
    - Referrer-Policy: controls referrer information
    - Cache-Control: prevents caching of sensitive data
    """
    
    def __init__(self):
        tools.Tool.__init__(
            self,
            on_start_resource=True,
            priority=100,  # Run early to ensure headers are set first
        )
    
    def _setup(self):
        def add_security_headers():
            response = cherrypy.response
            response.headers["X-Content-Type-Options"] = "nosniff"
            response.headers["X-Frame-Options"] = "DENY"
            response.headers["X-XSS-Protection"] = "1; mode=block"
            response.headers["Content-Security-Policy"] = "default-src 'self'"
            response.headers["Referrer-Policy"] = "strict-origin-when-cross-origin"
            response.headers["Cache-Control"] = "no-cache, no-store, must-revalidate"
        
        self.hook = add_security_headers


# Install security headers tool globally
security_headers_tool = SecurityHeadersTool()
security_headers_tool.subscribe()


# =============================================================================
# CONTROLLERS
# =============================================================================


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
        
        Raises:
            cherrypy.HTTPError: If ID validation fails.
        """
        # Security: Validate input - reject empty IDs
        if not user_id or not user_id.strip():
            if DEBUG_MODE:
                logger.debug("Invalid user ID: empty")
            raise cherrypy.HTTPError(
                400,
                "Bad Request: Missing or invalid ID parameter"
            )
        
        if DEBUG_MODE:
            logger.debug(f"User endpoint accessed with ID: {user_id}")
        return user_id

    @expose
    def POST(self) -> str:
        """
        Create a new user.
        
        Returns:
            str: Empty response for benchmarking.
        """
        if DEBUG_MODE:
            logger.debug("Create user endpoint accessed")
        # Security: Return 201 Created for POST requests
        cherrypy.response.status = 201
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
        if DEBUG_MODE:
            logger.debug("Root endpoint accessed")
        return ""


# =============================================================================
# APPLICATION CONFIGURATION
# =============================================================================


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

        # Global configuration - Production optimizations
        global_config = {
            "environment": "production" if IS_STANDALONE else "embedded",
            "log.screen": DEBUG_MODE,  # Disable screen logging in production
            "log.access_file": "",
            "log.error_file": "",
            "server.socket_host": "0.0.0.0",
            "server.socket_port": int(os.getenv("PORT", "3000")),
            "server.thread_pool": 30,
            "server.max_request_body_size": 16 * 1024 * 1024,  # 16 MB
            "response.timeout": 300,
            "engine.autoreload.on": DEBUG_MODE,
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


# =============================================================================
# ERROR HANDLING
# =============================================================================


# Error handling tool
@cherrypy.tool("before_error_response")
def handle_error() -> None:
    """
    Custom error handler for CherryPy.
    
    Security best practices:
    - Don't expose internal error details to clients
    - Log errors for debugging (but minimal in production)
    """
    if cherrypy.response.status.startswith("500"):
        if DEBUG_MODE:
            logger.exception(
                f"Server error: {cherrypy.response.status} - {cherrypy.request.path_info}",
                exc_info=cherrypy.request.error_traceback,
            )
        else:
            logger.error(
                f"Server error: {cherrypy.response.status} - {cherrypy.request.path_info}"
            )


handle_error.subscribe()


# =============================================================================
# APPLICATION INSTANCE
# =============================================================================

# Create application instance
app = BenchmarkApplication().mount()

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
    
    if not DEBUG_MODE:
        logger.warning("Starting CherryPy benchmark server in production mode")
    else:
        logger.info(f"Starting CherryPy benchmark server on port {port}")
    
    cherrypy.quickstart(app)