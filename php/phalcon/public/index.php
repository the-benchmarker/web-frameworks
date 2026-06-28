<?php

/**
 * Phalcon Framework Benchmark Server
 * 
 * A high-performance benchmark server using Phalcon framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

use Phalcon\Mvc\Micro;

$app = new Micro();

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
$app->get(
    '/',
    function () use ($app) {
        benchmark_log('Root endpoint accessed');
        $response = $app->response;
        $response->setStatusCode(200, "OK");
        $response->setContent("");
        $response->setHeader('Content-Type', 'text/plain');

        return $response;
    }
);

/**
 * Get user by ID
 * 
 * GET /user/{id}
 * 
 * @param string $id User identifier
 */
$app->get(
    '/user/{id}',
    function ($id) use ($app) {
        benchmark_log("User endpoint accessed with ID: {$id}");
        $response = $app->response;
        $response->setStatusCode(200, "OK");
        $response->setContent($id);
        $response->setHeader('Content-Type', 'text/plain');

        return $response;
    }
);

/**
 * Create new user
 * 
 * POST /user
 */
$app->post(
    '/user',
    function () use ($app) {
        benchmark_log('Create user endpoint accessed');
        $response = $app->response;
        $response->setStatusCode(200, "OK");
        $response->setContent("");
        $response->setHeader('Content-Type', 'text/plain');

        return $response;
    }
);

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 */
$app->get(
    '/health',
    function () use ($app) {
        benchmark_log('Health check endpoint accessed');
        $response = $app->response;
        $response->setStatusCode(200, "OK");
        $response->setContent("OK");
        $response->setHeader('Content-Type', 'text/plain');

        return $response;
    }
);

try {
    $app->handle($_SERVER["REQUEST_URI"]);
} catch (\Exception $e) {
    benchmark_log("Application Error: " . $e->getMessage() . "\n" . $e->getTraceAsString(), 'error');
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
}
