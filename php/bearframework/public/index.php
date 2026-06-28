<?php

/**
 * BearFramework Benchmark Server
 * 
 * A high-performance benchmark server using BearFramework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

use BearFramework\App;

require '../vendor/autoload.php';

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

$app = new App();

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
$app->routes->add('/', function () {
    benchmark_log('Root endpoint accessed');
    return (new App\Response(''))->header('Content-Type', 'text/plain');
});

/**
 * Get user by ID
 * 
 * GET /user/{id}
 */
$app->routes->add('/user/?', function (App\Request $request) {
    $id = $request->path->getSegment(1);
    benchmark_log("User endpoint accessed with ID: {$id}");
    return (new App\Response($id))->header('Content-Type', 'text/plain');
});

/**
 * Create new user
 * 
 * POST /user
 */
$app->routes->add('POST /user', function () {
    benchmark_log('Create user endpoint accessed');
    return (new App\Response(''))->header('Content-Type', 'text/plain');
});

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 */
$app->routes->add('/health', function () {
    benchmark_log('Health check endpoint accessed');
    return (new App\Response('OK'))->header('Content-Type', 'text/plain');
});

$app->run();
