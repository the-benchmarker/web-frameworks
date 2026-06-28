<?php

/**
 * HLEB2 Framework Benchmark Server Entry Point
 * 
 * A high-performance benchmark server using HLEB2 framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

use Hleb\HlebBootstrap;

define('HLEB_PUBLIC_DIR', realpath(__DIR__));
define('HLEB_GLOBAL_DIR', realpath(__DIR__ . '/../'));

require HLEB_GLOBAL_DIR . '/vendor/phphleb/framework/HlebBootstrap.php';

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

$config = [
    'common' => [
        'debug' => false,
        'allowed.hosts' => null,
        'log.enabled' => true, // Enable logging for benchmarking
        'max.log.level' => 'debug',
        'max.cli.log.level' => 'debug',
        'routes.auto-update' => true,
        'container.mock.allowed' => false,
        'app.cache.on' => false,
        'show.request.id' => false,
    ],
    'main' => [
        'session.enabled' => false,
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
 * Custom error handler
 */
set_error_handler(function ($code, $message, $file, $line) {
    benchmark_log("Error [{$code}]: {$message} in {$file} on line {$line}", 'error');
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
});

/**
 * Custom exception handler
 */
set_exception_handler(function ($exception) {
    benchmark_log("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString(), 'error');
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
});

try {
    (new HlebBootstrap(HLEB_PUBLIC_DIR, $config))->load();
} catch (\Exception $e) {
    benchmark_log("Application Error: " . $e->getMessage() . "\n" . $e->getTraceAsString(), 'error');
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
}
