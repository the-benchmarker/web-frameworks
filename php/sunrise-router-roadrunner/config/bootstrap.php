<?php

declare(strict_types=1);

// =============================================================================
// PRODUCTION CONFIGURATION FOR SUNRISE ROUTER ROADRUNNER
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

// Performance settings
ini_set('memory_limit', '256M');

use Symfony\Component\Dotenv\Dotenv;

require_once __DIR__ . '/../vendor/autoload.php';

// =============================================================================
// Security Headers and Logging
// =============================================================================

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

// Set error handlers
set_error_handler(function ($code, $message, $file, $line) {
    benchmark_log("Error [{$code}]: {$message} in {$file} on line {$line}", 'error');
});

set_exception_handler(function ($exception) {
    benchmark_log("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString(), 'error');
});

register_shutdown_function(function(): void {
    $error = error_get_last();
    if ($error !== null && $error['type'] === E_ERROR) {
        benchmark_log("Fatal Error: {$error['message']} in {$error['file']}:{$error['line']}", 'critical');
    }
});

(new Dotenv())->loadEnv(__DIR__ . '/../.env');
