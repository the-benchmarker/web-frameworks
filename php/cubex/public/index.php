<?php

/**
 * Cubex Framework Benchmark Server Entry Point
 * 
 * Production-grade benchmark server using Cubex framework.
 * Implements security best practices, proper error handling, and optimized logging.
 */

// =============================================================================
// PRODUCTION CONFIGURATION
// =============================================================================

error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('display_startup_errors', '0');
ini_set('log_errors', '1');
ini_set('log_errors_max_len', '1024');
ini_set('ignore_repeated_errors', '1');
ini_set('ignore_repeated_source', '1');
ini_set('html_errors', '0');

define('DEBUG_MODE', false);

define('PHP_START', microtime(true));

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

// Security settings
ini_set('expose_php', '0');
ini_set('session.cookie_httponly', '1');
ini_set('session.cookie_secure', '1');
ini_set('session.use_strict_mode', '1');

use Cubex\Cubex;
use App\Application;

/*
|--------------------------------------------------------------------------
| Security Headers Middleware
|--------------------------------------------------------------------------
*/

/**
 * Add security headers to response
 */
function addSecurityHeaders(): void {
    header('X-Content-Type-Options: nosniff');
    header('X-Frame-Options: DENY');
    header('X-XSS-Protection: 1; mode=block');
    header('Content-Security-Policy: default-src \'self\'');
    header('Referrer-Policy: strict-origin-when-cross-origin');
    header('Cache-Control: no-cache, no-store, must-revalidate');
    header('Pragma: no-cache');
    header('Expires: 0');
}

/*
|--------------------------------------------------------------------------
| Logging Setup
|--------------------------------------------------------------------------
*/

/**
 * Production-grade logger for benchmarking
 * 
 * @param string $message Log message
 * @param string $level Log level (debug, info, warning, error, critical)
 */
function benchmark_log(string $message, string $level = 'info'): void {
    if (!DEBUG_MODE && $level === 'debug') {
        return;
    }
    
    $timestamp = date('Y-m-d H:i:s');
    $logEntry = sprintf("[%s] %s - %s", $timestamp, strtoupper($level), $message);
    
    // Log to error log
    error_log($logEntry);
    
    // In production, don't log debug messages to slow down the application
    if (DEBUG_MODE) {
        // Additional debug output in development
        if ($level === 'error' || $level === 'critical') {
            fwrite(STDERR, $logEntry . PHP_EOL);
        }
    }
}

/*
|--------------------------------------------------------------------------
| Error Handling
|--------------------------------------------------------------------------
*/

/**
 * Production error handler
 */
function productionErrorHandler(int $errno, string $errstr, string $errfile = null, int $errline = null): bool {
    $errorLevels = [E_ERROR, E_PARSE, E_CORE_ERROR, E_COMPILE_ERROR, E_USER_ERROR];
    
    if (in_array($errno, $errorLevels, true)) {
        benchmark_log("Fatal Error: {$errstr} in {$errfile}:{$errline}", 'critical');
        http_response_code(500);
        header('Content-Type: text/plain');
        echo 'Internal Server Error';
        exit(1);
    }
    
    // Log warnings and notices in production
    if (!DEBUG_MODE && $errno !== E_NOTICE && $errno !== E_DEPRECATED && $errno !== E_USER_DEPRECATED) {
        benchmark_log("Warning: {$errstr} in {$errfile}:{$errline}", 'warning');
    }
    
    // In development, show all errors
    if (DEBUG_MODE) {
        benchmark_log("Error: {$errstr} in {$errfile}:{$errline}", 'error');
    }
    
    return false;
}

/**
 * Production exception handler
 */
function productionExceptionHandler(Throwable $e): void {
    addSecurityHeaders();
    benchmark_log("Application Error: " . $e->getMessage() . "\nStack Trace: " . $e->getTraceAsString(), 'error');
    
    if (DEBUG_MODE) {
        http_response_code(500);
        header('Content-Type: text/plain');
        echo "Error: " . $e->getMessage() . "\nFile: " . $e->getFile() . ":" . $e->getLine();
    } else {
        http_response_code(500);
        header('Content-Type: text/plain');
        echo 'Internal Server Error';
    }
}

// Set error handlers
set_error_handler('productionErrorHandler');
set_exception_handler('productionExceptionHandler');
register_shutdown_function(function(): void {
    $error = error_get_last();
    if ($error !== null && $error['type'] === E_ERROR) {
        productionErrorHandler($error['type'], $error['message'], $error['file'] ?? 'unknown', $error['line'] ?? 0);
    }
});

$loader = require_once(dirname(__DIR__) . '/vendor/autoload.php');

try {
    addSecurityHeaders();
    $cubex = new Cubex(dirname(__DIR__), $loader);
    $cubex->handle(new Application());
} catch(Throwable $e) {
    productionExceptionHandler($e);
} finally {
    if(isset($cubex) && $cubex instanceof Cubex) {
        $cubex->shutdown();
    }
}
