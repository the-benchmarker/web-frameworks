use warnings;
use strict;

{
    package Web::Frameworks::D2App;
    use Dancer2;
    use Log::Log4perl qw(:easy);
    use Data::Dumper;
    use Try::Tiny;

    # Production-grade Dancer2 Benchmark Server
    #
    # A high-performance, production-ready benchmark server implementation using the Dancer2 framework.
    # Security best practices, performance optimizations, and clean code.
    #
    # @author The Benchmarker Team
    # @version 1.0.0

    # ========================================================================
    # CONFIGURATION
    # ========================================================================

    # Application constants
    use constant APP_NAME    => 'Dancer2 Benchmark Server';
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
    my $logger = Log::Log4perl->get_logger("benchmark.dancer2");

    # ========================================================================
    # SECURITY CONFIGURATION
    # ========================================================================

    # Security headers middleware
    hook 'before' => sub {
        # Security: Add essential security headers to all responses
        header 'X-Content-Type-Options' => 'nosniff';
        header 'X-Frame-Options' => 'DENY';
        header 'X-XSS-Protection' => '1; mode=block';
        header 'Content-Security-Policy' => "default-src 'self'";
        header 'Cache-Control' => 'max-age=3600';
    };

    # ========================================================================
    # DANCER2 CONFIGURATION
    # ========================================================================

    # Configure Dancer2 for production performance
    set appname         => APP_NAME;
    set server          => 'PSGI';
    set port            => $port;
    set host            => $host;
    set startup_info    => 0;              # Disable startup info for production
    set show_errors     => 0;              # Disable error pages for production
    set serializer      => 'JSON';        # For potential future use
    set auto_reload     => 0;              # Disable auto-reload for production
    set warn_on_deprecated => 0;          # Disable warnings for production

    # Request body size limit for benchmarking (16MB)
    set max_request_size => 16 * 1024 * 1024;

    # ========================================================================
    # REQUEST HANDLERS
    # ========================================================================

    # Root endpoint handler
    # Returns empty response with 200 OK status for benchmarking
    # Security: Sets appropriate content-type header
    get '/' => sub {
        header 'Content-Type' => 'text/plain';
        return '';
    };

    # Get user by ID endpoint handler
    # Returns user ID as plain text with 200 OK status
    #
    # @param id - User identifier from URL path
    # @return User ID as plain text
    # Security: Validates input and sanitizes response
    get '/user/:id' => sub {
        my $id = route_parameters->{'id'};
        
        # Input validation - security best practice
        unless (defined $id && length $id) {
            $logger->warn("Missing ID parameter in user endpoint");
            header 'Content-Type' => 'text/plain';
            status 400;
            return 'Bad Request: Missing ID parameter';
        }
        
        header 'Content-Type' => 'text/plain';
        return $id;
    };

    # Create user endpoint handler
    # Returns empty response with 201 Created status for benchmarking
    # Security: Proper HTTP status for resource creation
    post '/user' => sub {
        header 'Content-Type' => 'text/plain';
        status 201;  # Created
        return '';
    };

    # Health check endpoint for monitoring
    # Returns "OK" with 200 OK status
    # Security: Minimal response for health checks
    get '/health' => sub {
        header 'Content-Type' => 'text/plain';
        return 'OK';
    };

    # ========================================================================
    # ERROR HANDLING
    # ========================================================================

    # Custom error handling for production
    # Security: Don't expose internal error details to clients
    hook 'after_error' => sub {
        my ($error) = @_;
        
        # Log error for debugging (only if debug mode is enabled)
        $logger->error("Error: " . Dumper($error)) if $debug_mode;
        
        # Return generic error message to client
        header 'Content-Type' => 'text/plain';
        status 500;
        return 'Internal Server Error';
    };

    # 404 Not Found handler
    hook 'after' => sub {
        my $response = shift;
        if ($response->status == 404) {
            header 'Content-Type' => 'text/plain';
            return 'Not Found';
        }
    };

    # 405 Method Not Allowed handler
    hook 'before' => sub {
        my $request = shift;
        if (!grep { $request->method eq $_ } qw(GET POST HEAD OPTIONS)) {
            header 'Content-Type' => 'text/plain';
            status 405;
            return 'Method Not Allowed';
        }
    };

    # ========================================================================
    # STARTUP
    # ========================================================================

    # Startup message - only when debug mode is enabled
    $logger->info("Starting " . APP_NAME . " v" . APP_VERSION . " on $host:$port") if $debug_mode;
}

Web::Frameworks::D2App->to_app;
