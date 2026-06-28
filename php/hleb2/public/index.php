<?php

/**
 * HLEB2 Framework Benchmark Server Entry Point
 * 
 * Production-grade benchmark server using HLEB2 framework.
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

// Security settings
ini_set('expose_php', '0');
ini_set('session.cookie_httponly', '1');
ini_set('session.cookie_secure', '1');

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

// Performance settings
ini_set('memory_limit', '256M');

use Hleb\HlebBootstrap;

define('HLEB_PUBLIC_DIR', realpath(__DIR__));
define('HLEB_GLOBAL_DIR', realpath(__DIR__ . '/../'));

require HLEB_GLOBAL_DIR . '/vendor/phphleb/framework/HlebBootstrap.php';

// =============================================================================
// Security Headers
// =============================================================================

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
function benchmark_log(string $message, string $level = 'debug'): void {
    if (!DEBUG_MODE && $level === 'debug') {
        return;
    }
    
    $timestamp = date('Y-m-d H:i:s');
    $logEntry = sprintf("[%s] %s - %s", $timestamp, strtoupper($level), $message);
    
    // Log to error log
    error_log($logEntry);
    
    // In production, don't log debug messages to slow down the application
    if (DEBUG_MODE) {
        if ($level === 'error' || $level === 'critical') {
            fwrite(STDERR, $logEntry . PHP_EOL);
        }
    }
}

$config = [
    'common' => [
        'debug' => DEBUG_MODE,
        'allowed.hosts' => null,
        'log.enabled' => true, // Enable logging for benchmarking
        'max.log.level' => DEBUG_MODE ? 'debug' : 'error',
        'max.cli.log.level' => DEBUG_MODE ? 'debug' : 'error',
        'routes.auto-update' => false,
        'container.mock.allowed' => false,
        'app.cache.on' => false,
        'show.request.id' => false,
    ],
    'main' => [
        'session.enabled' => false,
        'session.cookie_httponly' => true,
        'session.cookie_secure' => true,
    ],
    'system' => [
        'classes.autoload' => true,
        'classes.preload' => false,
        'events.used' => false,
        'async.clear.state' => false,
    ],
];

/*
|--------------------------------------------------------------------------
| Error Handling
|--------------------------------------------------------------------------
*/

/**
 * Production error handler
 */
set_error_handler(function ($code, $message, $file, $line) {
    benchmark_log("Error [{$code}]: {$message} in {$file} on line {$line}", 'error');
    addSecurityHeaders();
    http_response_code(500);
    header('Content-Type: text/plain');
    if (DEBUG_MODE) {
        echo "Error [{$code}]: {$message} in {$file} on line {$line}";
    } else {
        echo 'Internal Server Error';
    }
    exit;
});

/**
 * Production exception handler
 */
set_exception_handler(function ($exception) {
    benchmark_log("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString(), 'error');
    addSecurityHeaders();
    http_response_code(500);
    header('Content-Type: text/plain');
    if (DEBUG_MODE) {
        echo "Error: " . $exception->getMessage() . "\nFile: " . $exception->getFile() . ":" . $exception->getLine();
    } else {
        echo 'Internal Server Error';
    }
    exit;
});

register_shutdown_function(function(): void {
    $error = error_get_last();
    if ($error !== null && $error['type'] === E_ERROR) {
        benchmark_log("Fatal Error: {$error['message']} in {$error['file']}:{$error['line']}", 'critical');
        if (!DEBUG_MODE) {
            http_response_code(500);
            header('Content-Type: text/plain');
            echo 'Internal Server Error';
        }
    }
});

try {
    addSecurityHeaders();
    (new HlebBootstrap(HLEB_PUBLIC_DIR, $config))->load();
} catch (\Exception $e) {
    benchmark_log("Application Error: " . $e->getMessage() . "\n" . $e->getTraceAsString(), 'error');
    addSecurityHeaders();
    http_response_code(500);
    header('Content-Type: text/plain');
    if (DEBUG_MODE) {
        echo "Application Error: " . $e->getMessage();
    } else {
        echo 'Internal Server Error';
    }
    exit;
}
