<?php

/**
 * Flight Framework Benchmark Server
 * 
 * A high-performance benchmark server using Flight framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

require '../vendor/autoload.php';

/*
|--------------------------------------------------------------------------
| Configuration
|--------------------------------------------------------------------------
*/

// Configure Flight framework settings
Flight::set('flight.log_errors', true);
Flight::set('flight.handle_errors', false); // We'll handle errors ourselves

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

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
    Flight::json(['error' => 'Internal Server Error'], 500);
    exit;
});

/**
 * Custom exception handler
 */
set_exception_handler(function ($exception) {
    benchmark_log("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString(), 'error');
    Flight::json(['error' => 'Internal Server Error'], 500);
    exit;
});

// Override Flight's default error handling
Flight::map('error', function ($exception) {
    benchmark_log("Flight Error: " . $exception->getMessage(), 'error');
    header('Content-Type: text/plain');
    http_response_code(500);
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
Flight::route(
    'GET /',
    function () {
        benchmark_log('Root endpoint accessed');
        header('Content-Type: text/plain');
        echo '';
    }
);

/**
 * Get user by ID
 * 
 * GET /user/@id
 * 
 * @param string $id User identifier
 */
Flight::route(
    'GET /user/@id',
    function ($id) {
        benchmark_log("User endpoint accessed with ID: {$id}");
        header('Content-Type: text/plain');
        echo $id;
    }
);

/**
 * Create new user
 * 
 * POST /user
 */
Flight::route(
    'POST /user',
    function () {
        benchmark_log('Create user endpoint accessed');
        header('Content-Type: text/plain');
        echo '';
    }
);

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 */
Flight::route(
    'GET /health',
    function () {
        benchmark_log('Health check endpoint accessed');
        header('Content-Type: text/plain');
        echo 'OK';
    }
);

/*
|--------------------------------------------------------------------------
| 404 Handler
|--------------------------------------------------------------------------
*/
Flight::map('notFound', function () {
    benchmark_log('404 Not Found: ' . $_SERVER['REQUEST_URI'], 'error');
    header('Content-Type: text/plain');
    http_response_code(404);
    echo 'Not Found';
});

Flight::start();
