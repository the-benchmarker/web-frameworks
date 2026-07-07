<?php

/**
 * Production-grade Phalcon Framework Benchmark Server
 * 
 * A high-performance, production-ready benchmark server using Phalcon framework.
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
define('APP_NAME', 'Phalcon Benchmark Server');
define('APP_VERSION', '1.0.0');
define('DEBUG_MODE', false);

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

use Phalcon\Mvc\Micro;

$app = new Micro();

/*
|--------------------------------------------------------------------------
| Logging Setup
|--------------------------------------------------------------------------
*/

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

/*
|--------------------------------------------------------------------------
| Error Handling
|--------------------------------------------------------------------------
*/

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
$app->get(
    '/',
    function () use ($app) {
        addSecurityHeaders();
        benchmark_log('Root endpoint accessed');
        $response = $app->response;
        $response->setStatusCode(200, "OK");
        $response->setContent("");
        $response->setHeader('Content-Type', 'text/plain');

        return $response;
    }
);

/**
 * Get user by ID
 * 
 * GET /user/{id}
 * 
 * @param string $id User identifier
 */
$app->get(
    '/user/{id}',
    function ($id) use ($app) {
        addSecurityHeaders();
        // Input validation - security best practice
        if (empty($id)) {
            $response = $app->response;
            $response->setStatusCode(400, "Bad Request");
            $response->setContent('Bad Request: Missing ID parameter');
            $response->setHeader('Content-Type', 'text/plain');
            return $response;
        }
        benchmark_log("User endpoint accessed with ID: {$id}");
        $response = $app->response;
        $response->setStatusCode(200, "OK");
        $response->setContent($id);
        $response->setHeader('Content-Type', 'text/plain');

        return $response;
    }
);

/**
 * Create new user
 * 
 * POST /user
 */
$app->post(
    '/user',
    function () use ($app) {
        addSecurityHeaders();
        benchmark_log('Create user endpoint accessed');
        $response = $app->response;
        $response->setStatusCode(201, "Created");
        $response->setContent("");
        $response->setHeader('Content-Type', 'text/plain');

        return $response;
    }
);

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 */
$app->get(
    '/health',
    function () use ($app) {
        addSecurityHeaders();
        benchmark_log('Health check endpoint accessed');
        $response = $app->response;
        $response->setStatusCode(200, "OK");
        $response->setContent("OK");
        $response->setHeader('Content-Type', 'text/plain');

        return $response;
    }
);

try {
    addSecurityHeaders();
    $app->handle($_SERVER["REQUEST_URI"]);
} catch (\Exception $e) {
    benchmark_log("Application Error: " . $e->getMessage() . "\n" . $e->getTraceAsString(), 'error');
    addSecurityHeaders();
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
}
