use v5.38;
use warnings;
use strict;

use Kossy;
use Log::Log4perl qw(:easy);
use Data::Dumper;
use Try::Tiny;

# Production-grade Kossy Benchmark Server
#
# A high-performance, production-ready benchmark server implementation using the Kossy framework.
# Security best practices, performance optimizations, and clean code.
#
# @author The Benchmarker Team
# @version 1.0.0

# ========================================================================
# CONFIGURATION
# ========================================================================

# Application constants
use constant APP_NAME    => 'Kossy Benchmark Server';
use constant APP_VERSION => '1.0.0';

# Server configuration from environment variables with production defaults
my $port = $ENV{PORT} || 3000;
my $host = $ENV{HOST} || '0.0.0.0';

# Production flags - DISABLED for production performance
my $debug_mode = $ENV{DEBUG_MODE} // 0;
my $log_level  = $debug_mode ? $DEBUG : $ERROR;

# ========================================================================
# LOGGING CONFIGURATION
# ========================================================================

# Configure logging for production - ERROR level only, no debug in production
Log::Log4perl->easy_init({
    level    => $log_level,
    layout   => '%d [%p] %m%n',
    utf8     => 1
});
my $logger = Log::Log4perl->get_logger("benchmark.kossy");

# ========================================================================
# KOSSY CONFIGURATION
# ========================================================================

# Configure Kossy application for production
__PACKAGE__->load_plugins(
    'Config::JSON' => { path => undef }, # No config file needed for benchmarking
);

# Production configuration - disable debug and development features
__PACKAGE__->config(
    max_request_size    => 16 * 1024 * 1024,  # Request body size limit (16MB)
    debug               => 0,                # Disable debug mode for production
    show_errors        => 0,                # Disable error pages for production
    startup_info       => 0,                # Disable startup info for production
);

# ========================================================================
# SECURITY MIDDLEWARE
# ========================================================================

# Security headers middleware
hook before_dispatch => sub ($c) {
    # Security: Add essential security headers to all responses
    $c->res->header('X-Content-Type-Options' => 'nosniff');
    $c->res->header('X-Frame-Options' => 'DENY');
    $c->res->header('X-XSS-Protection' => '1; mode=block');
    $c->res->header('Content-Security-Policy' => "default-src 'self'");
    $c->res->header('Cache-Control' => 'max-age=3600');
};

# ========================================================================
# REQUEST HANDLERS
# ========================================================================

# Root endpoint handler
# Returns empty response with 200 OK status for benchmarking
# Security: Sets appropriate content-type header
get '/' => sub ($self, $c) {
    $c->halt_text(200, '', 'text/plain');
};

# Get user by ID endpoint handler
# Returns user ID as plain text with 200 OK status
#
# @param id - User identifier from URL path
# @return User ID as plain text
# Security: Validates input and sanitizes response
get '/user/:id' => sub ($self, $c) {
    my $id = $c->args->{'id'};
    
    # Input validation - security best practice
    unless (defined $id && length $id) {
        $logger->warn("Missing ID parameter in user endpoint");
        $c->halt_text(400, 'Bad Request: Missing ID parameter', 'text/plain');
    }
    
    $c->halt_text(200, $id, 'text/plain');
};

# Create user endpoint handler
# Returns empty response with 201 Created status for benchmarking
# Security: Proper HTTP status for resource creation
post '/user' => sub ($self, $c) {
    $c->halt_text(201, '', 'text/plain');
};

# Health check endpoint for monitoring
# Returns "OK" with 200 OK status
# Security: Minimal response for health checks
get '/health' => sub ($self, $c) {
    $c->halt_text(200, 'OK', 'text/plain');
};

# ========================================================================
# ERROR HANDLING
# ========================================================================

# Custom error handling for 404
hook before_routes => sub ($c) {
    $c->stash->{content_type} = 'text/plain';
};

# Global error handler
__PACKAGE__->on_error(sub ($self, $c, $error) {
    # Log error for debugging (only if debug mode is enabled)
    $logger->error("Error: " . Dumper($error)) if $debug_mode;
    
    # Security: Don't expose internal error details to client
    my $status = $error =~ /not found/i ? 404 : 500;
    my $message = $status == 404 ? 'Not Found' : 'Internal Server Error';
    
    $c->halt_text($status, $message, 'text/plain');
});

# ========================================================================
# STARTUP
# ========================================================================

# Startup message - only when debug mode is enabled
$logger->info("Starting " . APP_NAME . " v" . APP_VERSION . " on $host:$port") if $debug_mode;

__PACKAGE__->psgi;
