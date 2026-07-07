<?php

/**
 * Production-grade FatFree Framework Benchmark Server
 * 
 * A high-performance, production-ready benchmark server using FatFree framework.
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

// Production constants
define('APP_NAME', 'FatFree Benchmark Server');
define('APP_VERSION', '1.0.0');
define('DEBUG_MODE', false);

require '../vendor/autoload.php';

$f3 = Base::instance();

// ============================================================================
// SECURITY CONFIGURATION
// ============================================================================

// Configure request body size limit (16 MB)
$f3->set('UPLOADS', 16 * 1024 * 1024);
$f3->set('MAXGRAPH', 16 * 1024 * 1024);

// Configure logging for production
$f3->set('LOG', 'error'); // Log errors to PHP error log
$f3->set('DEBUG', 0); // Disable debug output for benchmarking

// ============================================================================
// SECURITY HEADERS MIDDLEWARE
// ============================================================================

// Add security headers to all responses
$f3->before('GET,POST,PUT,DELETE,PATCH,HEAD,OPTIONS *', function() {
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

// ============================================================================
// ROUTES
// ============================================================================

/**
 * Root endpoint handler
 * 
 * GET /
 */
$f3->route(
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
 * @param Base $f3 FatFree framework instance
 * Security: Validates input
 */
$f3->route(
    'GET /user/@id',
    function ($f3) {
        $id = $f3->get('PARAMS.id');
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
$f3->route(
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
$f3->route(
    'GET /health',
    function () {
        benchmark_log('Health check endpoint accessed');
        header('Content-Type: text/plain');
        echo 'OK';
    }
);

// ============================================================================
// ERROR HANDLERS
// ============================================================================

// Custom ONERROR handler for both 404 and 405
$f3->set('ONERROR', function ($f3) {
    $errorCode = $f3->get('ERROR.code');
    benchmark_log($f3->get('ERROR.code') . ' Error: ' . $f3->get('PATH'), 'error');
    
    switch ($errorCode) {
        case 404:
            header('Content-Type: text/plain');
            http_response_code(404);
            echo 'Not Found';
            break;
        case 405:
            header('Content-Type: text/plain');
            http_response_code(405);
            echo 'Method Not Allowed';
            break;
        default:
            header('Content-Type: text/plain');
            http_response_code(500);
            echo 'Internal Server Error';
            break;
    }
});

// ============================================================================
// STARTUP
// ============================================================================

$f3->run();
