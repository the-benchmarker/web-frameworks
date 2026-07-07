<?php

/**
 * Production-grade Workerman Framework Benchmark Server
 * 
 * A high-performance, production-ready benchmark server using Workerman framework.
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
define('APP_NAME', 'Workerman Benchmark Server');
define('APP_VERSION', '1.0.0');
define('DEBUG_MODE', false);

require_once __DIR__ . '/vendor/autoload.php';

use Workerman\Worker;

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

$worker = new Worker('http://0.0.0.0:3000');
$worker->count = shell_exec('nproc') ?: 32;

// ============================================================================
// SECURITY HEADERS MIDDLEWARE
// ============================================================================

/**
 * Add security headers to response
 * Security best practice: Add security headers to all responses
 */
function addSecurityHeaders(): array {
    return [
        'X-Content-Type-Options' => 'nosniff',
        'X-Frame-Options' => 'DENY',
        'X-XSS-Protection' => '1; mode=block',
        'Content-Security-Policy' => "default-src 'self'",
        'Cache-Control' => 'max-age=3600'
    ];
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
    // For Workerman, we can't exit, but we can log and continue
});

/**
 * Custom exception handler for production
 * Security: Don't expose internal error details
 */
set_exception_handler(function ($exception) {
    benchmark_log("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString(), 'error');
    // For Workerman, we can't exit, but we can log and continue
});

$worker->onMessage = static function ($connection, $request) {
    $path = $request->path();
    
    // Log requests for benchmarking
    $clientIp = $connection->getRemoteIp();
    $method = $request->method();
    
    // Get security headers
    $securityHeaders = addSecurityHeaders();
    
    switch ($path) {
        case '/':
            benchmark_log("Root endpoint accessed from {$clientIp}");
            $connection->send('', array_merge(['Content-Type' => 'text/plain'], $securityHeaders));
            break;
            
        case '/user':
            benchmark_log("Create user endpoint accessed from {$clientIp}");
            $connection->send('', array_merge(['Content-Type' => 'text/plain'], $securityHeaders), 201); // Created
            break;
            
        case '/health':
            benchmark_log("Health check endpoint accessed from {$clientIp}");
            $connection->send('OK', array_merge(['Content-Type' => 'text/plain'], $securityHeaders));
            break;
            
        default:
            if (\str_starts_with($path, '/user/')) {
                $id = \substr($path, 6);
                // Input validation - security best practice
                if (empty($id)) {
                    benchmark_log("Bad Request: Missing ID parameter from {$clientIp}", 'error');
                    $connection->send('Bad Request: Missing ID parameter', array_merge(['Content-Type' => 'text/plain'], $securityHeaders), 400);
                    break;
                }
                benchmark_log("User endpoint accessed with ID: {$id} from {$clientIp}");
                $connection->send($id, array_merge(['Content-Type' => 'text/plain'], $securityHeaders));
            } else {
                benchmark_log("404 Not Found: {$path} from {$clientIp}", 'error');
                $connection->send('Not Found', array_merge(['Content-Type' => 'text/plain'], $securityHeaders), 404);
            }
            break;
    }
};

Worker::runAll();
