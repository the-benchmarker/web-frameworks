<?php

/**
 * Mark Framework Benchmark Server
 * 
 * A high-performance benchmark server using Mark framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

use Mark\App;

require __DIR__.'/../vendor/autoload.php';

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

$api = new App('http://0.0.0.0:3000');

$api->count = shell_exec('nproc') ?: 8;

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
$api->get('/', fn() => '');

/**
 * Create new user
 * 
 * POST /user
 */
$api->post('/user', fn() => '');

/**
 * Get user by ID
 * 
 * GET /user/{id}
 * 
 * @param mixed $request Request object
 * @param string $id User identifier
 * @return string User ID
 */
$api->get('/user/{id}', fn($request, $id) => $id);

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 */
$api->get('/health', fn() => 'OK');

$api->start();
