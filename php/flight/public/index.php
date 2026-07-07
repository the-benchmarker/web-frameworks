<?php

/**
 * Production-grade Flight Framework Benchmark Server
 * 
 * A high-performance, production-ready benchmark server using Flight framework.
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

// Production constants
define('APP_NAME', 'Flight Benchmark Server');
define('APP_VERSION', '1.0.0');
define('DEBUG_MODE', false);

require '../vendor/autoload.php';

// ============================================================================
// SECURITY CONFIGURATION
// ============================================================================

// Configure Flight framework settings for production
Flight::set('flight.log_errors', false);  // Disable Flight's internal logging
Flight::set('flight.handle_errors', false); // We'll handle errors ourselves

// ============================================================================
// SECURITY HEADERS MIDDLEWARE
// ============================================================================

// Add security headers to all responses
Flight::before('start', function () {
    header('X-Content-Type-Options: nosniff');
    header('X-Frame-Options: DENY');
    header('X-XSS-Protection: 1; mode=block');
    header('Content-Security-Policy: default-src \'self\'');
    header('Cache-Control: max-age=3600');
});

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

/*
|--------------------------------------------------------------------------
| Logging Setup
|--------------------------------------------------------------------------
*/

/**
 * Custom logger for benchmarking
 * 
 * @param string $message Log message
 * @param string $level Log level (debug, info, error)
 */
function benchmark_log(string $message, string $level = 'debug'): void {
    $timestamp = date('Y-m-d H:i:s');
    error_log("[{$timestamp}] {$level} - {$message}");
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

// Override Flight's default error handling for production
Flight::map('error', function ($exception) {
    benchmark_log("Flight Error: " . $exception->getMessage(), 'error');
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
 */
Flight::route(
    'GET /',
    function () {
        benchmark_log('Root endpoint accessed');
        header('Content-Type: text/plain');
        echo '';
    }
);

/**
 * Get user by ID
 * 
 * GET /user/@id
 * 
 * @param string $id User identifier
 * Security: Validates input
 */
Flight::route(
    'GET /user/@id',
    function ($id) {
        benchmark_log("User endpoint accessed with ID: {$id}");
        
        // Input validation - security best practice
        if (empty($id)) {
            header('Content-Type: text/plain');
            http_response_code(400);
            echo 'Bad Request: Missing ID parameter';
            exit;
        }
        
        header('Content-Type: text/plain');
        echo $id;
    }
);

/**
 * Create new user
 * 
 * POST /user
 * Returns 201 Created status for resource creation
 */
Flight::route(
    'POST /user',
    function () {
        benchmark_log('Create user endpoint accessed');
        header('Content-Type: text/plain');
        http_response_code(201); // Created
        echo '';
    }
);

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 */
Flight::route(
    'GET /health',
    function () {
        benchmark_log('Health check endpoint accessed');
        header('Content-Type: text/plain');
        echo 'OK';
    }
);

// ============================================================================
// 404 Handler
// ============================================================================
Flight::map('notFound', function () {
    benchmark_log('404 Not Found: ' . $_SERVER['REQUEST_URI'], 'error');
    header('Content-Type: text/plain');
    http_response_code(404);
    echo 'Not Found';
});

// ============================================================================
// 405 Method Not Allowed Handler
// ============================================================================
Flight::map('methodNotAllowed', function () {
    header('Content-Type: text/plain');
    http_response_code(405);
    echo 'Method Not Allowed';
});

// ============================================================================
// STARTUP
// ============================================================================

Flight::start();
