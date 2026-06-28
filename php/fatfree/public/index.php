<?php

/**
 * FatFree Framework Benchmark Server
 * 
 * A high-performance benchmark server using FatFree framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

require '../vendor/autoload.php';

$f3 = Base::instance();

/*
|--------------------------------------------------------------------------
| Configuration
|--------------------------------------------------------------------------
*/

// Configure request body size limit (16 MB)
$f3->set('UPLOADS', 16 * 1024 * 1024);
$f3->set('MAXGRAPH', 16 * 1024 * 1024);

// Configure logging
$f3->set('LOG', 'error'); // Log errors to PHP error log
$f3->set('DEBUG', 0); // Disable debug output for benchmarking

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
    echo 'Internal Server Error';
    http_response_code(500);
    header('Content-Type: text/plain');
    exit;
});

/**
 * Custom exception handler
 */
set_exception_handler(function ($exception) {
    benchmark_log("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString(), 'error');
    echo 'Internal Server Error';
    http_response_code(500);
    header('Content-Type: text/plain');
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
$f3->route(
    'GET /',
    function () {
        benchmark_log('Root endpoint accessed');
        echo '';
    }
);

/**
 * Get user by ID
 * 
 * GET /user/@id
 * 
 * @param Base $f3 FatFree framework instance
 */
$f3->route(
    'GET /user/@id',
    function ($f3) {
        $id = $f3->get('PARAMS.id');
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
$f3->route(
    'POST /user',
    function () {
        benchmark_log('Create user endpoint accessed');
        echo '';
    }
);

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 */
$f3->route(
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
$f3->set('ONERROR', function ($f3) {
    benchmark_log('404 Not Found: ' . $f3->get('PATH'), 'error');
    header('Content-Type: text/plain');
    http_response_code(404);
    echo 'Not Found';
});

$f3->run();
