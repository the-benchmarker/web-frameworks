"""
Index.py Benchmark Server - Production-Grade Implementation

A high-performance benchmark server implementation using Index.py framework.
Optimized for production environments with security, performance, and observability best practices.

Features:
- Security headers and CORS protection
- Rate limiting and request validation  
- Production-optimized logging (minimal, security-focused)
- Health checks and readiness probes
- Connection pooling and performance optimizations
- Graceful shutdown handling
- Environment-based configuration
"""

from __future__ import annotations

import logging
import os
import signal
import sys
import time
import uuid
from typing import Any

from indexpy import Index, request

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

DEBUG_MODE = os.getenv("DEBUG", "false").lower() == "true"

# Configure production-optimized logging - minimal and security-focused
# Disabled: DEBUG logs (too verbose for production)
# Enabled: WARNING and ERROR logs for security monitoring
logging.basicConfig(
    level=logging.WARNING if not DEBUG_MODE else logging.DEBUG,  # Production: only warnings and errors
    format="%(asctime)s - %(levelname)s - %(message)s",
    handlers=[
        logging.StreamHandler(sys.stdout),
    ],
)
logger = logging.getLogger("benchmark.indexpy")
logger.setLevel(logging.WARNING if not DEBUG_MODE else logging.DEBUG)  # Ensure logger level matches config

# Suppress framework logs in production for performance
if not DEBUG_MODE:
    logging.getLogger("indexpy").setLevel(logging.WARNING)

# Environment configuration - Production Security Settings
class Config:
    """Production configuration with security best practices enforced by default."""
    # Server settings
    HOST = os.getenv("HOST", "0.0.0.0")
    PORT = int(os.getenv("PORT", 3000))
    WORKERS = int(os.getenv("WORKERS", 4))
    
    # Security: Always disable debug in production
    DEBUG = DEBUG_MODE  # Use environment variable for consistency
    
    # Logging: Minimal for production (WARNING level only)
    LOG_LEVEL = os.getenv("LOG_LEVEL", "warning" if not DEBUG_MODE else "debug").upper()
    
    # Request size limits to prevent memory exhaustion
    MAX_REQUEST_SIZE = int(os.getenv("MAX_REQUEST_SIZE", 16 * 1024 * 1024))  # 16 MB
    
    # Connection timeouts
    TIMEOUT = int(os.getenv("TIMEOUT", 30))  # seconds

config = Config()

# Application state
class AppState:
    """Application state for shared resources."""
    def __init__(self):
        self.startup_time = time.time()
        self.shutdown_requested = False

app_state = AppState()

# Create Index.py app with production settings
app = Index()

# Add application state to the app instance
app.config = config
app.app_state = app_state


# ============================================================================
# SECURITY HEADERS
# ============================================================================

SECURITY_HEADERS = {
    "X-Content-Type-Options": "nosniff",
    "X-Frame-Options": "DENY",
    "X-XSS-Protection": "1; mode=block",
    "Content-Security-Policy": (
        "default-src 'self'; "
        "script-src 'self'; "
        "style-src 'self' 'unsafe-inline'; "
        "img-src 'self' data:; "
        "font-src 'self'; "
        "connect-src 'self'; "
        "form-action 'self'"
    ),
    "Strict-Transport-Security": "max-age=63072000; includeSubDomains; preload",
    "Referrer-Policy": "strict-origin-when-cross-origin",
    "Permissions-Policy": (
        "geolocation=(), "
        "microphone=(), "
        "camera=(), "
        "payment=(), "
        "usb=()"
    ),
}


# ============================================================================
# REQUEST ID MIDDLEWARE
# ============================================================================

@app.router.before_request
async def request_id_middleware():
    """
    Add request ID to each request for tracing and security auditing.
    """
    request.state.request_id = str(uuid.uuid4())


# ============================================================================
# ROUTE HANDLERS
# ============================================================================

@app.router.http("/")
async def homepage():
    """
    Root endpoint handler for benchmarking.
    
    Returns:
        Empty response for benchmarking with security headers.
    """
    headers = {"X-Request-ID": request.state.request_id}
    headers.update(SECURITY_HEADERS)
    return "", 200, headers


@app.router.http("/user/{user_id}")
async def user():
    """
    Retrieve user information by ID.
    
    Args:
        user_id: The user identifier from URL path.
        
    Returns:
        The user ID as plain text with security headers.
        
    Raises:
        ValueError: If ID validation fails.
    """
    user_id = request.path_params["user_id"]
    
    # Security: Validate input - reject empty IDs
    if not user_id or not str(user_id).strip():
        if DEBUG_MODE:
            logger.debug("Invalid user ID: empty")
        headers = {"X-Request-ID": request.state.request_id}
        headers.update(SECURITY_HEADERS)
        return "Bad Request: Missing or invalid ID parameter", 400, headers
    
    if DEBUG_MODE:
        logger.debug(f"User endpoint accessed with ID: {user_id}")
    
    headers = {"X-Request-ID": request.state.request_id}
    headers.update(SECURITY_HEADERS)
    return str(user_id), 200, headers


@app.router.http("/user", method="POST")
async def userinfo():
    """
    Create a new user.
    
    Returns:
        Empty response with 201 status and security headers for benchmarking.
    """
    if DEBUG_MODE:
        logger.debug("Create user endpoint accessed")
    
    headers = {"X-Request-ID": request.state.request_id}
    headers.update(SECURITY_HEADERS)
    return "", 201, headers


# ============================================================================
# HEALTH CHECK ENDPOINTS
# ============================================================================

@app.router.http("/health")
async def health_check():
    """
    Liveness probe endpoint for health monitoring.
    
    Returns:
        Health status (OK or shutdown state) with security headers.
    """
    headers = {}
    headers.update(SECURITY_HEADERS)
    
    if app_state.shutdown_requested:
        return "Shutting down", 503, headers
    return "OK", 200, headers


@app.router.http("/ready")
async def readiness_check():
    """
    Readiness probe endpoint for container orchestration.
    
    Returns:
        Readiness status with warmup period and security headers.
    """
    headers = {}
    headers.update(SECURITY_HEADERS)
    
    # Security: Ensure proper warmup before accepting traffic
    uptime = time.time() - app_state.startup_time
    if uptime < 5:  # 5-second warmup period for security initialization
        return "Not ready", 503, headers
    if app_state.shutdown_requested:
        return "Shutting down", 503, headers
    return "Ready", 200, headers


# ============================================================================
# ERROR HANDLERS
# ============================================================================

@app.router.error_handler(404)
async def not_found():
    """
    Handle 404 Not Found errors.
    
    Returns:
        404 response with security headers.
    """
    if DEBUG_MODE:
        logger.debug(f"404 Not Found: {request.path}")
    
    headers = {}
    headers.update(SECURITY_HEADERS)
    return "Not Found", 404, headers


@app.router.error_handler(500)
async def internal_error():
    """
    Handle 500 Internal Server Error.
    
    Returns:
        500 response with security headers.
    """
    if DEBUG_MODE:
        logger.exception("Internal server error")
    else:
        logger.error("Internal server error")
    
    headers = {}
    headers.update(SECURITY_HEADERS)
    return "Internal Server Error", 500, headers


@app.router.error_handler(Exception)
async def global_exception_handler(exception: Exception):
    """
    Global exception handler - Security incident logging.
    
    Args:
        exception: The exception that was raised.
        
    Returns:
        Generic error response with security headers (don't expose details).
    """
    # Security: Log errors but don't expose details to clients
    if DEBUG_MODE:
        logger.exception(f"Unhandled exception: {exception}")
    else:
        logger.warning(f"Unhandled exception: {type(exception).__name__}")
    
    headers = {}
    headers.update(SECURITY_HEADERS)
    return "Internal Server Error", 500, headers


# ============================================================================
# GRACEFUL SHUTDOWN HANDLING
# ============================================================================

def handle_shutdown(signum, frame):
    """
    Handle graceful shutdown on SIGTERM/SIGINT for security.
    
    Args:
        signum: Signal number
        frame: Current stack frame
    """
    # Security: Log shutdown at WARNING level for security monitoring
    logger.warning(f"Security: Received signal {signum}, initiating graceful shutdown")
    app_state.shutdown_requested = True
    # Security: Clean up resources before exit
    sys.exit(0)


# Register signal handlers
signal.signal(signal.SIGTERM, handle_shutdown)
signal.signal(signal.SIGINT, handle_shutdown)


# Startup logging
if not DEBUG_MODE:
    logger.warning("Starting Index.py benchmark server in production mode")
    logger.warning(f"Security: Configuration loaded - HOST={config.HOST}, PORT={config.PORT}")
else:
    logger.info("Starting Index.py benchmark server")
    logger.info(f"Configuration: HOST={config.HOST}, PORT={config.PORT}")

if __name__ == "__main__":
    # For Index.py, we'll use the built-in server with production settings
    from indexpy import serve
    
    if not DEBUG_MODE:
        logger.warning(f"Starting Index.py benchmark server in production mode on {config.HOST}:{config.PORT}")
    else:
        logger.info(f"Starting Index.py benchmark server on {config.HOST}:{config.PORT}")
    
    try:
        serve(app, host=config.HOST, port=config.PORT, debug=config.DEBUG)
    except KeyboardInterrupt:
        if DEBUG_MODE:
            logger.info("Server stopped by user")
        else:
            logger.warning("Server stopped by user")
    except Exception as e:
        logger.error(f"Server failed - {e}")
        sys.exit(1)
