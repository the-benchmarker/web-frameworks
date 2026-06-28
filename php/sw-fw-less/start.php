<?php

/**
 * SW Framework Less Benchmark Server Entry Point
 * 
 * A high-performance benchmark server using SW Framework Less.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

!defined('APP_BASE_PATH') && define('APP_BASE_PATH', __DIR__ . '/');

if (extension_loaded('jsonnet')) {
    !defined('CONFIG_FORMAT') && define('CONFIG_FORMAT', 'array,jsonnet');
}

require_once __DIR__ . '/vendor/autoload.php';

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

/*
|--------------------------------------------------------------------------
| Error Handling
|--------------------------------------------------------------------------
*/

/**
 * Custom error handler
 */
set_error_handler(function ($code, $message, $file, $line) {
    benchmark_log("Error [{$code}]: {$message} in {$file} on line {$line}", 'error');
    // For swoole-based frameworks, we can't exit, but we can log
});

/**
 * Custom exception handler
 */
set_exception_handler(function ($exception) {
    benchmark_log("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString(), 'error');
    // For swoole-based frameworks, we can't exit, but we can log
});

try {
    //This app supports hot reload and shutdown triggered by SIGTERM
    (new \SwFwLess\bootstrap\App())->run();
} catch (\Exception $e) {
    benchmark_log("Application Error: " . $e->getMessage() . "\n" . $e->getTraceAsString(), 'error');
    // For swoole-based frameworks, we can't exit, but we can log
}
