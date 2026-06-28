<?php

/**
 * Production-grade Laravel Framework Benchmark Server
 * 
 * A high-performance, production-ready benchmark server using Laravel framework.
 * Security best practices, performance optimizations, and clean code.
 * 
 * @package  Laravel
 * @author   Taylor Otwell <taylor@laravel.com>
 * @author   The Benchmarker Team
 * @version  1.0.0
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
define('APP_NAME', 'Laravel Benchmark Server');
define('APP_VERSION', '1.0.0');
define('DEBUG_MODE', false);

define('LARAVEL_START', microtime(true));

/*
|--------------------------------------------------------------------------
| Register The Auto Loader
|--------------------------------------------------------------------------
|
| Composer provides a convenient, automatically generated class loader for
| our application. We just need to utilize it! We'll simply require it
| into the script here so that we don't have to worry about manual
| loading any of our classes later on. It feels great to relax.
|
*/

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

// ============================================================================
// PRODUCTION ERROR HANDLING
// ============================================================================

/**
 * Custom error handler for production
 * Security: Don't expose internal error details
 */
set_error_handler(function ($code, $message, $file, $line) {
    if (DEBUG_MODE) {
        error_log("[" . date('Y-m-d H:i:s') . "] ERROR - Error [{$code}]: {$message} in {$file} on line {$line}");
    }
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
    if (DEBUG_MODE) {
        error_log("[" . date('Y-m-d H:i:s') . "] ERROR - Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString());
    }
    addSecurityHeaders();
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
});

/*
|--------------------------------------------------------------------------
| Turn On The Lights
|--------------------------------------------------------------------------
|
| We need to illuminate PHP development, so let us turn on the lights.
| This bootstraps the framework and gets it ready for use, then it
| will load up this application so that we can run it and send
| the responses back to the browser and delight our users.
|
*/

// Set production environment variables
$_SERVER['APP_ENV'] = 'production';
$_SERVER['APP_DEBUG'] = 'false';

$app = require_once __DIR__.'/../bootstrap/app.php';

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

/*
|--------------------------------------------------------------------------
| Run The Application
|--------------------------------------------------------------------------
|
| Once we have the application, we can handle the incoming request
| through the kernel, and send the associated response back to
| the client's browser allowing them to enjoy the creative
| and wonderful application we have prepared for them.
|
*/

addSecurityHeaders();

$kernel = $app->make(Illuminate\Contracts\Http\Kernel::class);

$response = $kernel->handle(
    $request = Illuminate\Http\Request::capture()
);

$response->send();

$kernel->terminate($request, $response);
