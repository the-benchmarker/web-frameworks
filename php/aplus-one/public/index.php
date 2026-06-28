<?php

/**
 * APlus One Framework Benchmark Server Entry Point
 * 
 * A high-performance benchmark server using APlus One framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

require __DIR__ . '/../vendor/autoload.php';

use Framework\MVC\App;
use Framework\Routing\RouteCollection;
use Framework\Routing\Router;

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

try {
    (new App([
        'router' => [
            'default' => [
                'callback' => function (Router $router) {
                    $router->serve(null, function (RouteCollection $routes) {
                        /**
                         * Root endpoint handler
                         * GET /
                         */
                        $routes->get('/', fn () => '');
                        
                        /**
                         * Get user by ID
                         * GET /user/{int}
                         */
                        $routes->get('/user/{int}', function ($args) {
                            benchmark_log("User endpoint accessed with ID: {$args[0]}");
                            return $args[0];
                        });
                        
                        /**
                         * Create new user
                         * POST /user
                         */
                        $routes->post('/user', function () {
                            benchmark_log('Create user endpoint accessed');
                            return '';
                        });
                        
                        /**
                         * Health check endpoint for monitoring
                         * GET /health
                         */
                        $routes->get('/health', function () {
                            benchmark_log('Health check endpoint accessed');
                            return 'OK';
                        });
                    });
                },
            ],
        ],
    ]))->runHttp();
} catch (\Exception $e) {
    benchmark_log("Application Error: " . $e->getMessage() . "\n" . $e->getTraceAsString(), 'error');
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
}
