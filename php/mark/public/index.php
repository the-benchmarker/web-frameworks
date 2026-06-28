<?php

/**
 * Production-grade Mark Framework Benchmark Server
 * 
 * A high-performance, production-ready benchmark server using Mark framework.
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
define('APP_NAME', 'Mark Benchmark Server');
define('APP_VERSION', '1.0.0');
define('DEBUG_MODE', false);

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

use Mark\App;

require __DIR__.'/../vendor/autoload.php';

// ============================================================================
// SECURITY HEADERS MIDDLEWARE
// ============================================================================

/**
 * Add security headers to response
 * Security best practice: Add security headers to all responses
 */
function addSecurityHeaders(): void {
    header('X-Content-Type-Options: nosniff');
    header('X-Frame-Options: DENY');
    header('X-XSS-Protection: 1; mode=block');
    header("Content-Security-Policy: default-src 'self'");
    header('Cache-Control: max-age=3600');
}

/*
|--------------------------------------------------------------------------
| Logging Setup
|--------------------------------------------------------------------------
*/

/**
 * Custom logger for benchmarking
 * Production: Only log errors when not in debug mode
 * 
 * @param string $message Log message
 * @param string $level Log level (debug, info, error)
 */
function benchmark_log(string $message, string $level = 'debug'): void {
    if (DEBUG_MODE || $level === 'error') {
        $timestamp = date('Y-m-d H:i:s');
        error_log("[{$timestamp}] {$level} - {$message}");
    }
}

// ============================================================================
// PRODUCTION ERROR HANDLING
// ============================================================================

/**
 * Custom error handler for production
 * Security: Don't expose internal error details
 */
set_error_handler(function ($code, $message, $file, $line) {
    benchmark_log("Error [{$code}]: {$message} in {$file} on line {$line}", 'error');
    addSecurityHeaders();
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
});

/**
 * Custom exception handler for production
 * Security: Don't expose internal error details
 */
set_exception_handler(function ($exception) {
    benchmark_log("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString(), 'error');
    addSecurityHeaders();
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
});

$api = new App('http://0.0.0.0:3000');

$api->count = shell_exec('nproc') ?: 8;

/*
|--------------------------------------------------------------------------
| Routes
|--------------------------------------------------------------------------
*/

/**
 * Root endpoint handler
 * 
 * GET /
 */
$api->get('/', function () {
    addSecurityHeaders();
    benchmark_log('Root endpoint accessed');
    return '';
});

/**
 * Create new user
 * 
 * POST /user
 */
$api->post('/user', function () {
    addSecurityHeaders();
    benchmark_log('Create user endpoint accessed');
    http_response_code(201); // Created
    return '';
});

/**
 * Get user by ID
 * 
 * GET /user/{id}
 * 
 * @param mixed $request Request object
 * @param string $id User identifier
 * @return string User ID
 * Security: Validates input
 */
$api->get('/user/{id}', function ($request, $id) {
    addSecurityHeaders();
    // Input validation - security best practice
    if (empty($id)) {
        http_response_code(400);
        header('Content-Type: text/plain');
        return 'Bad Request: Missing ID parameter';
    }
    benchmark_log("User endpoint accessed with ID: {$id}");
    return $id;
});

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 */
$api->get('/health', function () {
    addSecurityHeaders();
    benchmark_log('Health check endpoint accessed');
    return 'OK';
});

// ============================================================================
// STARTUP
// ============================================================================

$api->start();
