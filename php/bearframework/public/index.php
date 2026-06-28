<?php

/**
 * Production-grade BearFramework Benchmark Server
 * 
 * A high-performance, production-ready benchmark server using BearFramework.
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
// Performance: Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');
// Performance: Increase memory limit for production
ini_set('memory_limit', '256M');

use BearFramework\App;

require '../vendor/autoload.php';

// Production constants
define('APP_NAME', 'BearFramework Benchmark Server');
define('APP_VERSION', '1.0.0');
define('DEBUG_MODE', false);

$app = new App();

// ============================================================================
// PRODUCTION LOGGING
// ============================================================================

/**
 * Production-grade logger - only logs in debug mode
 * 
 * @param string $message Log message
 * @param string $level Log level (debug, info, error)
 */
function benchmark_log(string $message, string $level = 'debug'): void {
    if (DEBUG_MODE) {
        $timestamp = date('Y-m-d H:i:s');
        error_log("[{$timestamp}] {$level} - {$message}");
    }
}

// ============================================================================
// ERROR HANDLING
// ============================================================================

/**
 * Custom error handler for production
 * Security: Don't expose internal error details
 */
set_error_handler(function ($code, $message, $file, $line) {
    benchmark_log("Error [{$code}]: {$message} in {$file} on line {$line}", 'error');
    header('Content-Type: text/plain');
    http_response_code(500);
    echo 'Internal Server Error';
    exit;
});

/**
 * Custom exception handler for production
 * Security: Don't expose internal error details
 */
set_exception_handler(function ($exception) {
    benchmark_log("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString(), 'error');
    header('Content-Type: text/plain');
    http_response_code(500);
    echo 'Internal Server Error';
    exit;
});

// ============================================================================
// SECURITY HEADERS MIDDLEWARE
// ============================================================================

// Add security headers middleware
$app->middleware->add(function (App\Request $request, callable $next) {
    $response = $next($request);
    return $response
        ->header('X-Content-Type-Options', 'nosniff')
        ->header('X-Frame-Options', 'DENY')
        ->header('X-XSS-Protection', '1; mode=block')
        ->header('Content-Security-Policy', "default-src 'self'")
        ->header('Cache-Control', 'max-age=3600');
});

// ============================================================================
// ROUTES
// ============================================================================

/**
 * Root endpoint handler
 * 
 * GET /
 */
$app->routes->add('/', function () {
    benchmark_log('Root endpoint accessed');
    return (new App\Response(''))->header('Content-Type', 'text/plain');
});

/**
 * Get user by ID
 * 
 * GET /user/{id}
 * Security: Validates input
 */
$app->routes->add('/user/?', function (App\Request $request) {
    $id = $request->path->getSegment(1);
    benchmark_log("User endpoint accessed with ID: {$id}");
    
    // Input validation - security best practice
    if (empty($id)) {
        return (new App\Response('Bad Request: Missing ID parameter'))
            ->header('Content-Type', 'text/plain')
            ->statusCode(400);
    }
    
    return (new App\Response($id))->header('Content-Type', 'text/plain');
});

/**
 * Create new user
 * 
 * POST /user
 * Returns 201 Created status for resource creation
 */
$app->routes->add('POST /user', function () {
    benchmark_log('Create user endpoint accessed');
    return (new App\Response(''))->header('Content-Type', 'text/plain')->statusCode(201);
});

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 */
$app->routes->add('/health', function () {
    benchmark_log('Health check endpoint accessed');
    return (new App\Response('OK'))->header('Content-Type', 'text/plain');
});

// ============================================================================
// ERROR HANDLERS
// ============================================================================

// 404 and 405 handler
$app->routes->add('*', function (App\Request $request) {
    $path = $request->path->string;
    $method = $request->method;
    
    // Check if it's a 405 (Method Not Allowed)
    if (!in_array($method, ['GET', 'POST', 'HEAD', 'OPTIONS']) && 
        $path !== '/' && !str_starts_with($path, '/user/') && $path !== '/health') {
        return (new App\Response('Method Not Allowed'))
            ->header('Content-Type', 'text/plain')
            ->statusCode(405);
    }
    
    // Default 404
    return (new App\Response('Not Found'))
        ->header('Content-Type', 'text/plain')
        ->statusCode(404);
});

// ============================================================================
// STARTUP
// ============================================================================

$app->run();
