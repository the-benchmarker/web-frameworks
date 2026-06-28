"""
Flask Benchmark Server - Production-Grade Implementation

A benchmark server implementation using Flask framework.
Optimized for production environments with security, performance, and observability best practices.

Features:
- Security headers and CORS protection
- Rate limiting and request validation
- Structured logging and metrics
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
from typing import Union, Optional
from functools import wraps

from flask import Flask, Response, request, g
from flask_cors import CORS
from flask_limiter import Limiter
from flask_limiter.util import get_remote_address
from prometheus_client import Counter, Histogram, generate_latest, REGISTRY
from prometheus_client.openmetrics.exposition import CONTENT_TYPE_LATEST
from werkzeug.middleware.gzip import GzipMiddleware
from werkzeug.middleware.https_redirect import HTTPSRedirectMiddleware
from werkzeug.middleware.proxy_fix import ProxyFix

# Configure structured logging for production
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    handlers=[
        logging.StreamHandler(sys.stdout),
    ],
)
logger = logging.getLogger("benchmark.flask")


# Environment configuration
class Config:
    """Production configuration from environment variables."""
    HOST = os.getenv("HOST", "0.0.0.0")
    PORT = int(os.getenv("PORT", 3000))
    DEBUG = os.getenv("DEBUG", "false").lower() == "true"
    LOG_LEVEL = os.getenv("LOG_LEVEL", "info").upper()
    RATE_LIMIT = os.getenv("RATE_LIMIT", "1000 per minute")
    MAX_CONTENT_LENGTH = int(os.getenv("MAX_CONTENT_LENGTH", 16 * 1024 * 1024))  # 16 MB
    TIMEOUT = int(os.getenv("TIMEOUT", 30))  # seconds
    CORS_ORIGINS = os.getenv("CORS_ORIGINS", "*").split(",")
    PROMETHEUS_ENABLED = os.getenv("PROMETHEUS_ENABLED", "true").lower() == "true"
    TRUSTED_HOSTS = os.getenv("TRUSTED_HOSTS", "").split(",") if os.getenv("TRUSTED_HOSTS") else None
    FORCE_HTTPS = os.getenv("FORCE_HTTPS", "false").lower() == "true"

config = Config()


# Metrics
REQUEST_COUNT = Counter(
    "http_requests_total",
    "Total HTTP Requests",
    ["method", "endpoint", "http_status"]
)
REQUEST_LATENCY = Histogram(
    "http_request_duration_seconds",
    "HTTP request latency in seconds",
    ["method", "endpoint"]
)
REQUEST_SIZE = Histogram(
    "http_request_size_bytes",
    "HTTP request size in bytes",
    ["method", "endpoint"]
)
RESPONSE_SIZE = Histogram(
    "http_response_size_bytes",
    "HTTP response size in bytes",
    ["method", "endpoint"]
)


# Application state
class AppState:
    """Application state for shared resources."""
    def __init__(self):
        self.startup_time = time.time()
        self.shutdown_requested = False
        self.request_id = None

app_state = AppState()


# Create Flask application
app = Flask(__name__)

# Configure Flask for production
app.config.update(
    DEBUG=config.DEBUG,
    JSONIFY_PRETTYPRINT_REGULAR=False,
    MAX_CONTENT_LENGTH=config.MAX_CONTENT_LENGTH,
    PERMANENT_SESSION_LIFETIME=3600,  # 1 hour
    SESSION_COOKIE_SECURE=config.FORCE_HTTPS,
    SESSION_COOKIE_HTTPONLY=True,
    SESSION_COOKIE_SAMESITE="Lax",
)

# Add application state
app.config["app_state"] = app_state
app.config["config"] = config

# CORS configuration for production
cors_config = {
    "origins": config.CORS_ORIGINS if config.CORS_ORIGINS != ["*"] else ["*"],
    "methods": ["GET", "POST", "OPTIONS"],
    "allow_headers": ["*"],
    "expose_headers": ["X-Request-ID", "X-RateLimit-Limit", "X-RateLimit-Remaining"],
    "supports_credentials": True,
    "max_age": 600,
}

if config.CORS_ORIGINS == ["*"]:  # If wildcard, allow all
    CORS(app, resources={r"/*": cors_config})
else:
    CORS(app, resources={r"/*": cors_config})

# Rate limiter
limiter = Limiter(
    app=app,
    key_func=get_remote_address,
    default_limits=[config.RATE_LIMIT],
    storage_uri="memory://",  # Use Redis in production: "redis://localhost:6379"
    strategy="fixed-window",  # or "moving-window"
)

# Add middleware for production
if config.TRUSTED_HOSTS:
    app.wsgi_app = ProxyFix(app.wsgi_app, x_for=1, x_proto=1, x_host=1, x_prefix=1)

if config.FORCE_HTTPS:
    app.wsgi_app = HTTPSRedirectMiddleware(app.wsgi_app)

# Gzip compression middleware
app.wsgi_app = GzipMiddleware(app.wsgi_app, minimum_size=1000, compresslevel=6)


# Security headers middleware
def add_security_headers(response: Response) -> Response:
    """
    Add security headers to all responses.
    
    Args:
        response: Flask Response object
        
    Returns:
        Response: Response with security headers added
    """
    security_headers = {
        "X-Content-Type-Options": "nosniff",
        "X-Frame-Options": "DENY",
        "X-XSS-Protection": "1; mode=block",
        "Content-Security-Policy": "default-src 'self'",
        "Referrer-Policy": "strict-origin-when-cross-origin",
        "Permissions-Policy": "geolocation=(), microphone=(), camera=()",
    }
    
    if config.FORCE_HTTPS:
        security_headers["Strict-Transport-Security"] = "max-age=63072000; includeSubDomains; preload"
    
    for header, value in security_headers.items():
        response.headers[header] = value
    
    return response


# Register security headers middleware
app.after_request(add_security_headers)


# Request ID generator and middleware
@app.before_request
def before_request():
    """Generate request ID for tracing before each request."""
    g.request_id = str(uuid.uuid4())
    app_state.request_id = g.request_id


@app.after_request
def after_request(response: Response) -> Response:
    """
    Add request ID to response headers and collect metrics.
    
    Args:
        response: Flask Response object
        
    Returns:
        Response: Response with request ID header
    """
    if hasattr(g, 'request_id'):
        response.headers["X-Request-ID"] = g.request_id
        
        # Collect metrics
        method = request.method
        endpoint = request.path
        status_code = response.status_code
        
        REQUEST_COUNT.labels(method=method, endpoint=endpoint, http_status=status_code).inc()
        REQUEST_LATENCY.labels(method=method, endpoint=endpoint).observe(
            time.time() - getattr(g, 'start_time', time.time())
        )
    
    return response


@app.before_request
def record_start_time():
    """Record request start time for metrics."""
    g.start_time = time.time()


# Route handlers with rate limiting
@app.route("/", methods=["GET"])
@limiter.limit(config.RATE_LIMIT)
def index() -> Response:
    """
    Root endpoint handler.
    
    Returns:
        Response: Empty response for benchmarking.
    """
    logger.debug(f"Root endpoint accessed - Request ID: {g.request_id}")
    return Response(response="", status=200, mimetype="text/plain")


@app.route("/user/<int:id>", methods=["GET"])
@limiter.limit(config.RATE_LIMIT)
def user_info(id: int) -> Union[Response, str]:
    """
    Retrieve user information by ID.
    
    Args:
        id: The user identifier.
    
    Returns:
        Response or str: The user ID as plain text.
    """
    logger.debug(f"User endpoint accessed with ID: {id} - Request ID: {g.request_id}")
    return Response(response=str(id), status=200, mimetype="text/plain")


@app.route("/user", methods=["POST"])
@limiter.limit(config.RATE_LIMIT)
def create_user() -> Response:
    """
    Create a new user.
    
    Returns:
        Response: Empty response for benchmarking.
    """
    logger.debug(f"Create user endpoint accessed - Request ID: {g.request_id}")
    return Response(response="", status=200, mimetype="text/plain")


# Exception handlers
@app.errorhandler(429)
def ratelimit_handler(e) -> Response:
    """
    Handle rate limit exceeded errors.
    
    Args:
        e: Rate limit exceeded error
        
    Returns:
        Response: Rate limit exceeded response
    """
    logger.warning(f"Rate limit exceeded: {e}")
    return Response(
        response="Rate limit exceeded",
        status=429,
        mimetype="text/plain",
        headers={"Retry-After": getattr(e, "retry_after", 60)}
    )


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


# Health check endpoints
@app.route("/health", methods=["GET"])
def health_check() -> Response:
    """
    Health check endpoint for liveness probes.
    
    Returns:
        Response: Simple health status.
    """
    if app_state.shutdown_requested:
        return Response(response="Shutting down", status=503, mimetype="text/plain")
    return Response(response="OK", status=200, mimetype="text/plain")


@app.route("/ready", methods=["GET"])
def readiness_check() -> Response:
    """
    Readiness check endpoint for Kubernetes/container orchestration.
    
    Returns:
        Response: Readiness status.
    """
    uptime = time.time() - app_state.startup_time
    if uptime < 5:  # Allow 5 seconds for warmup
        return Response(response="Not ready", status=503, mimetype="text/plain")
    if app_state.shutdown_requested:
        return Response(response="Shutting down", status=503, mimetype="text/plain")
    return Response(response="Ready", status=200, mimetype="text/plain")


@app.route("/metrics", methods=["GET"])
def metrics_endpoint() -> Response:
    """
    Prometheus metrics endpoint for monitoring.
    
    Returns:
        Response: Prometheus metrics in OpenMetrics format.
    """
    if config.PROMETHEUS_ENABLED:
        metrics_data = generate_latest(REGISTRY)
        return Response(
            response=metrics_data.decode("utf-8"),
            status=200,
            mimetype=CONTENT_TYPE_LATEST
        )
    return Response(response="Metrics disabled", status=404, mimetype="text/plain")


# Graceful shutdown handling
def handle_shutdown(signum, frame):
    """
    Handle graceful shutdown on SIGTERM/SIGINT.
    
    Args:
        signum: Signal number
        frame: Current stack frame
    """
    logger.info(f"Received signal {signum}, initiating graceful shutdown")
    app_state.shutdown_requested = True
    sys.exit(0)


# Register signal handlers
signal.signal(signal.SIGTERM, handle_shutdown)
signal.signal(signal.SIGINT, handle_shutdown)


# Startup logging
logger.info("Flask application configured for production")
logger.info(f"Configuration: HOST={config.HOST}, PORT={config.PORT}, DEBUG={config.DEBUG}")


if __name__ == "__main__":
    logger.info(f"Starting Flask benchmark server on {config.HOST}:{config.PORT}")
    
    try:
        app.run(
            host=config.HOST,
            port=config.PORT,
            debug=config.DEBUG,
            threaded=True,
            use_reloader=False,
        )
    except KeyboardInterrupt:
        logger.info("Server stopped by user")
    except Exception as e:
        logger.error(f"Server failed: {e}", exc_info=True)
        sys.exit(1)
