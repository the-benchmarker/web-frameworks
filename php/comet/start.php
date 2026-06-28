<?php

/**
 * Comet Framework Benchmark Server
 * 
 * A high-performance benchmark server using Comet framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

require_once __DIR__ . '/vendor/autoload.php';

! defined('WORKERS_COUNT') && define('WORKERS_COUNT', shell_exec('nproc'));

use Comet\Comet;

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

$app = new Comet([
    'port'    => 3000,
    'workers' => intval(WORKERS_COUNT),
]);

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
$app->get('/', function ($request, $response) {
    benchmark_log('Root endpoint accessed');
    return $response->withHeader('Content-Type', 'text/plain');
});

/**
 * Create new user
 * 
 * POST /user
 */
$app->post('/user', function ($request, $response) {
    benchmark_log('Create user endpoint accessed');
    return $response->withHeader('Content-Type', 'text/plain');
});

/**
 * Get user by ID
 * 
 * GET /user/{id}
 */
$app->get('/user/{id}', function ($request, $response, $args) {
    $id = $args['id'];
    benchmark_log("User endpoint accessed with ID: {$id}");
    return $response->with($id)->withHeader('Content-Type', 'text/plain');
});

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 */
$app->get('/health', function ($request, $response) {
    benchmark_log('Health check endpoint accessed');
    return $response->with('OK')->withHeader('Content-Type', 'text/plain');
});

$app->run();
