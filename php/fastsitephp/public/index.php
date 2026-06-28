<?php

/**
 * Production-grade FastSitePHP Framework Benchmark Server
 * 
 * A high-performance, production-ready benchmark server using FastSitePHP framework.
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
define('APP_NAME', 'FastSitePHP Benchmark Server');
define('APP_VERSION', '1.0.0');
define('DEBUG_MODE', false);

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

// -----------------------------------------------
// Load Dependencies (additional PHP files)
// -----------------------------------------------

// Setup a PHP Autoloader
// This allows classes to be dynamically loaded and is included when
// dependencies are installed through the PHP Package Manager Composer.
//
// require __DIR__ . '/../vendor/autoload.php';

// Or for a minimal site when using FastSitePHP only the following 2 files
// need to be included.
//
// Using location based on install from:
//     composer require fastsitephp/fastsitephp
//
// This also assumes that this file is located under a [public]
// directory (or directory with another name and same dir structure)
//
require __DIR__ . '/../vendor/fastsitephp/fastsitephp/src/Application.php';
require __DIR__ . '/../vendor/fastsitephp/fastsitephp/src/Route.php';

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

// -----------------------------------------------
// Create the setup the Application Object with
// Error Handling and UTC for the Timezone
// -----------------------------------------------

$app = new \FastSitePHP\Application();
$app->setup('UTC');

// -----------------------------------------------
// Define Routes
// -----------------------------------------------

/**
 * Root endpoint handler
 * 
 * GET /
 */
$app->get('/', function () {
    addSecurityHeaders();
    benchmark_log('Root endpoint accessed');
    return '';
});

/**
 * Get user by ID
 * 
 * GET /user/{id}
 * 
 * @param string $id User identifier
 * Security: Validates input
 */
$app->get('/user/:id', function ($id) use ($app) {
    addSecurityHeaders();
    // Input validation - security best practice
    if (empty($id)) {
        http_response_code(400);
        header('Content-Type: text/plain');
        return 'Bad Request: Missing ID parameter';
    }
    benchmark_log("User endpoint accessed with ID: {$id}");
    // Safely escape the user input since it's returned to the client.
    return $app->escape($id);
});

/**
 * Create new user
 * 
 * POST /user
 */
$app->post('/user', function () {
    addSecurityHeaders();
    benchmark_log('Create user endpoint accessed');
    http_response_code(201); // Created
    return '';
});

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 */
$app->get('/health', function () {
    addSecurityHeaders();
    benchmark_log('Health check endpoint accessed');
    return 'OK';
});

// ============================================================================
// STARTUP
// ============================================================================

$app->run();
