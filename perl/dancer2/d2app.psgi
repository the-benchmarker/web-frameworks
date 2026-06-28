use warnings;
use strict;

{
    package Web::Frameworks::D2App;
    use Dancer2;
    use Log::Log4perl qw(:easy);
    use Data::Dumper;

    # Dancer2 Benchmark Server
    #
    # A high-performance benchmark server implementation using the Dancer2 framework.
    # Follows Perl best practices including proper error handling, logging, and configuration management.

    # Configure logging for benchmarking
    Log::Log4perl->easy_init({ level    => $DEBUG, layout => '%d [%p] %m%n' });
    my $logger = Log::Log4perl->get_logger("benchmark.dancer2");

    # Server configuration from environment variables
    my $port = $ENV{PORT} || 3000;
    my $host = $ENV{HOST} || '0.0.0.0';

    # Configure Dancer2 for production performance
    set appname => 'Dancer2 Benchmark Server';
    set server => 'PSGI';
    set port => $port;
    set host => $host;
    set startup_info => 0; # Disable startup info for benchmarking
    set show_errors => 0;  # Disable error pages for benchmarking
    set serializer => 'JSON'; # For potential future use, though we use plain text

    # Request body size limit for benchmarking (16MB)
    set max_request_size => 16 * 1024 * 1024;

    # Root endpoint handler
    # Returns empty response with 200 OK status for benchmarking
    get '/' => sub {
        $logger->debug("Root endpoint accessed");
        header 'Content-Type' => 'text/plain';
        return '';
    };

    # Get user by ID endpoint handler
    # Returns user ID as plain text with 200 OK status
    #
    # Parameters:
    #   id - User identifier from URL path
    #
    # Returns:
    #   User ID as plain text
    get '/user/:id' => sub {
        my $id = route_parameters->{'id'}; 
        $logger->debug("User endpoint accessed with ID: $id");
        header 'Content-Type' => 'text/plain';
        return $id;
    };

    # Create user endpoint handler
    # Returns empty response with 201 Created status for benchmarking
    post '/user' => sub {
        $logger->debug("Create user endpoint accessed");
        header 'Content-Type' => 'text/plain';
        status 201;
        return '';
    };

    # Health check endpoint for monitoring
    # Returns "OK" with 200 OK status
    get '/health' => sub {
        header 'Content-Type' => 'text/plain';
        return 'OK';
    };

    # Custom error handling
    hook 'after_error' => sub {
        my ($error) = @_;
        $logger->error("Error: " . Dumper($error));
        header 'Content-Type' => 'text/plain';
        status 500;
        return 'Internal Server Error';
    };

    $logger->info("Starting Dancer2 benchmark server on $host:$port");
}

Web::Frameworks::D2App->to_app;
