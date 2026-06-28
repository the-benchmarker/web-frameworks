<?php

/**
 * Lemon Framework Benchmark Server
 * 
 * A high-performance benchmark server using Lemon framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

use Lemon\Kernel\Application;
use Lemon\Protection\Middlwares\Csrf;
use Lemon\ResponseFactory;
use Lemon\Route;

require __DIR__.'/../vendor/autoload.php';

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

Application::init(__DIR__);

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
Route::get('/', function () {
    benchmark_log('Root endpoint accessed');
    header('Content-Type: text/plain');
    return '';
});

/**
 * Get user by ID
 * 
 * GET /user/{id}
 * 
 * @param string $id User identifier
 */
Route::get('/user/{id}', function ($id) {
    benchmark_log("User endpoint accessed with ID: {$id}");
    header('Content-Type: text/plain');
    return $id;
});

/**
 * Create new user
 * 
 * POST /user
 */
Route::post('/user', function () {
    benchmark_log('Create user endpoint accessed');
    header('Content-Type: text/plain');
    return '';
})->exclude(Csrf::class); // Since Lemon by default checks csrf, we have to manually disable it.

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 */
Route::get('/health', function () {
    benchmark_log('Health check endpoint accessed');
    header('Content-Type: text/plain');
    return 'OK';
})->exclude(Csrf::class);

// 404 handler
Route::fallback(function () {
    benchmark_log('404 Not Found: ' . $_SERVER['REQUEST_URI'], 'error');
    http_response_code(404);
    header('Content-Type: text/plain');
    return 'Not Found';
});
