use v5.38;
use warnings;
use strict;

use Kossy;
use Log::Log4perl qw(:easy);
use Data::Dumper;

# Kossy Benchmark Server
#
# A high-performance benchmark server implementation using the Kossy framework.
# Follows Perl best practices including proper error handling, logging, and configuration management.

# Configure logging for benchmarking
Log::Log4perl->easy_init({ level    => $DEBUG, layout => '%d [%p] %m%n' });
my $logger = Log::Log4perl->get_logger("benchmark.kossy");

# Server configuration from environment variables
my $port = $ENV{PORT} || 3000;
my $host = $ENV{HOST} || '0.0.0.0';

# Configure Kossy application
__PACKAGE__->load_plugins(
    'Config::JSON' => { path => undef }, # No config file needed for benchmarking
);

# Request body size limit for benchmarking (16MB)
__PACKAGE__->config(max_request_size => 16 * 1024 * 1024);

# Root endpoint handler
# Returns empty response with 200 OK status for benchmarking
get '/' => sub ($self, $c) {
    $logger->debug("Root endpoint accessed");
    $c->halt_text(200, '', 'text/plain');
};

# Get user by ID endpoint handler
# Returns user ID as plain text with 200 OK status
#
# Parameters:
#   id - User identifier from URL path
#
# Returns:
#   User ID as plain text
get '/user/:id' => sub ($self, $c) {
    my $id = $c->args->{'id'}; 
    $logger->debug("User endpoint accessed with ID: $id");
    $c->halt_text(200, $id, 'text/plain');
};

# Create user endpoint handler
# Returns empty response with 201 Created status for benchmarking
post '/user' => sub ($self, $c) {
    $logger->debug("Create user endpoint accessed");
    $c->halt_text(201, '', 'text/plain');
};

# Health check endpoint for monitoring
# Returns "OK" with 200 OK status
get '/health' => sub ($self, $c) {
    $c->halt_text(200, 'OK', 'text/plain');
};

# Custom error handling for 404
hook before_routes => sub ($c) {
    $c->stash->{content_type} = 'text/plain';
};

# Log startup information
$logger->info("Starting Kossy benchmark server on $host:$port");

__PACKAGE__->psgi;
