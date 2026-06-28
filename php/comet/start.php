<?php

/**
 * Production-grade Comet Framework Benchmark Server
 * 
 * A high-performance, production-ready benchmark server using Comet framework.
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
define('APP_NAME', 'Comet Benchmark Server');
define('APP_VERSION', '1.0.0');
define('DEBUG_MODE', false);

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

require_once __DIR__ . '/vendor/autoload.php';

! defined('WORKERS_COUNT') && define('WORKERS_COUNT', shell_exec('nproc'));

use Comet\Comet;

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

$app = new Comet([
    'port'    => 3000,
    'workers' => intval(WORKERS_COUNT),
]);

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
$app->get('/', function ($request, $response) {
    addSecurityHeaders();
    benchmark_log('Root endpoint accessed');
    return $response->withHeader('Content-Type', 'text/plain');
});

/**
 * Create new user
 * 
 * POST /user
 */
$app->post('/user', function ($request, $response) {
    addSecurityHeaders();
    benchmark_log('Create user endpoint accessed');
    return $response->withStatus(201)->withHeader('Content-Type', 'text/plain'); // Created
});

/**
 * Get user by ID
 * 
 * GET /user/{id}
 * Security: Validates input
 */
$app->get('/user/{id}', function ($request, $response, $args) {
    addSecurityHeaders();
    $id = $args['id'];
    // Input validation - security best practice
    if (empty($id)) {
        return $response->withStatus(400)->with('Bad Request: Missing ID parameter')->withHeader('Content-Type', 'text/plain');
    }
    benchmark_log("User endpoint accessed with ID: {$id}");
    return $response->with($id)->withHeader('Content-Type', 'text/plain');
});

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 */
$app->get('/health', function ($request, $response) {
    addSecurityHeaders();
    benchmark_log('Health check endpoint accessed');
    return $response->with('OK')->withHeader('Content-Type', 'text/plain');
});

$app->run();
