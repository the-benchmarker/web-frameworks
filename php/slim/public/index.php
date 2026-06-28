<?php

/**
 * Production-grade Slim Framework Benchmark Server
 * 
 * A high-performance, production-ready benchmark server using Slim framework.
 * Security best practices, performance optimizations, and clean code.
 * 
 * @author The Benchmarker Team
 * @version 1.0.0
 */

// ============================================================================
// PRODUCTION CONFIGURATION
// ============================================================================

// Security: Disable error display in production
ini_set('display_errors', '0');
// Security: Disable expose PHP version
ini_set('expose_php', '0');
// Performance: Only log errors, not warnings or notices
ini_set('log_errors', '1');
// Performance: Increase memory limit for production
ini_set('memory_limit', '256M');

use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use Psr\Log\LoggerInterface;
use Slim\Factory\AppFactory;
use Slim\Logger;

require __DIR__ . '/../vendor/autoload.php';

// Production constants
define('APP_NAME', 'Slim Benchmark Server');
define('APP_VERSION', '1.0.0');
define('DEBUG_MODE', false);

// ============================================================================
// APPLICATION SETUP
// ============================================================================

$app = AppFactory::create();

// Configure body parsing and limits
$app->addBodyParsingMiddleware();
$app->addRoutingMiddleware();

// Configure error middleware for production
$app->addErrorMiddleware(false, false, false); // Disable debug in production

// ============================================================================
// CONFIGURATION
// ============================================================================

// Configure request body size limit (16 MB) for production
$app->getContainer()->set('settings', function () {
    return [
        'displayErrorDetails' => false,  // Never show error details in production
        'logErrorDetails' => true,
        'logErrors' => true,
        'determineRouteBeforeAppMiddleware' => true,
        'upload_max_filesize' => '16M',
        'post_max_size' => '16M',
    ];
});

// ============================================================================
// PRODUCTION LOGGING
// ============================================================================

// Configure logger for production (only errors)
$app->getContainer()->set(LoggerInterface::class, function () {
    $logger = new Logger([
        'name' => 'benchmark-slim',
        'path' => 'php://stdout',
        'level' => DEBUG_MODE ? \Monolog\Logger::DEBUG : \Monolog\Logger::ERROR,
    ]);
    
    $logger->pushHandler(new \Monolog\Handler\StreamHandler('php://stdout', DEBUG_MODE ? \Monolog\Logger::DEBUG : \Monolog\Logger::ERROR));
    return $logger;
});

/**
 * Get logger instance
 */
$logger = $app->getContainer()->get(LoggerInterface::class);

// ============================================================================
// SECURITY HEADERS MIDDLEWARE
// ============================================================================

// Add security headers middleware
$app->add(function ($request, $handler) {
    $response = $handler->handle($request);
    return $response
        ->withHeader('X-Content-Type-Options', 'nosniff')
        ->withHeader('X-Frame-Options', 'DENY')
        ->withHeader('X-XSS-Protection', '1; mode=block')
        ->withHeader('Content-Security-Policy', "default-src 'self'")
        ->withHeader('Cache-Control', 'max-age=3600');
});

// ============================================================================
// ERROR HANDLING
// ============================================================================

/**
 * Custom error handler for production
 * Security: Don't expose internal error details
 */
$customErrorHandler = function (Request $request, \Throwable $exception, bool $displayErrorDetails) use ($logger) {
    $logger->error('Error: ' . $exception->getMessage() . '\n' . $exception->getTraceAsString());
    
    $response = $app->getResponseFactory()->createResponse();
    $response->getBody()->write('Internal Server Error');
    
    return $response->withStatus(500)->withHeader('Content-Type', 'text/plain');
};

$app->addErrorMiddleware(false, false, false, $customErrorHandler);

/**
 * PHP error handler for production
 * Security: Don't expose internal error details
 */
set_error_handler(function ($code, $message, $file, $line) use ($logger) {
    $logger->error("Error [{$code}]: {$message} in {$file} on line {$line}");
    header('Content-Type: text/plain');
    http_response_code(500);
    echo 'Internal Server Error';
    exit;
});

/**
 * PHP exception handler for production
 * Security: Don't expose internal error details
 */
set_exception_handler(function ($exception) use ($logger) {
    $logger->error("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString());
    header('Content-Type: text/plain');
    http_response_code(500);
    echo 'Internal Server Error';
    exit;
});

// ============================================================================
// ROUTES
// ============================================================================

/**
 * Root endpoint handler
 * 
 * GET /
 * 
 * @param Request $request HTTP request object
 * @param Response $response HTTP response object
 * @return Response Empty response for benchmarking
 */
$app->get('/', function (Request $request, Response $response) use ($logger): Response {
    $logger->debug('Root endpoint accessed');
    $response->getBody()->write('');
    return $response->withHeader('Content-Type', 'text/plain');
});

/**
 * Get user by ID
 * 
 * GET /user/{id}
 * 
 * @param Request $request HTTP request object
 * @param Response $response HTTP response object
 * @param array $args Route arguments
 * @return Response User ID as plain text
 * Security: Validates input
 */
$app->get('/user/{id}', function (Request $request, Response $response, array $args) use ($logger): Response {
    $id = $args['id'] ?? '';
    $logger->debug("User endpoint accessed with ID: {$id}");
    
    // Input validation - security best practice
    if (empty($id)) {
        $response->getBody()->write('Bad Request: Missing ID parameter');
        return $response->withStatus(400)->withHeader('Content-Type', 'text/plain');
    }
    
    $response->getBody()->write($id);
    return $response->withHeader('Content-Type', 'text/plain');
});

/**
 * Create new user
 * 
 * POST /user
 * 
 * @param Request $request HTTP request object
 * @param Response $response HTTP response object
 * @return Response Empty response with 201 Created status
 */
$app->post('/user', function (Request $request, Response $response) use ($logger): Response {
    $logger->debug('Create user endpoint accessed');
    $response->getBody()->write('');
    return $response->withStatus(201)->withHeader('Content-Type', 'text/plain'); // Created
});

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 * 
 * @param Request $request HTTP request object
 * @param Response $response HTTP response object
 * @return Response Health status
 */
$app->get('/health', function (Request $request, Response $response) use ($logger): Response {
    $logger->debug('Health check endpoint accessed');
    $response->getBody()->write('OK');
    return $response->withHeader('Content-Type', 'text/plain');
});

// ============================================================================
// STARTUP
// ============================================================================

$app->run();
