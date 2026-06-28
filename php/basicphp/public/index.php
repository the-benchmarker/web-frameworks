<?php

/**
 * BasicPHP Benchmark Server
 * 
 * A high-performance benchmark server using BasicPHP framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

require_once __DIR__ . '/../Basic.php'; // BasicPHP class library

/*
|--------------------------------------------------------------------------
| Configuration
|--------------------------------------------------------------------------
*/

// Configure request body size limit
Basic::set('MAX_CONTENT_LENGTH', 16 * 1024 * 1024); // 16 MB

/*
|--------------------------------------------------------------------------
| Logging Setup
|--------------------------------------------------------------------------
*/

// Configure logging for benchmarking
Basic::set('LOG_LEVEL', 'debug');
Basic::set('LOG_FORMAT', '[{timestamp}] {level} - {message}');

/*
|--------------------------------------------------------------------------
| Error Handling
|--------------------------------------------------------------------------
*/

/**
 * Custom error handler for the benchmark application
 * 
 * @param int $code Error code
 * @param string $message Error message
 * @param string $file File where error occurred
 * @param int $line Line number where error occurred
 */
set_error_handler(function ($code, $message, $file, $line) {
    Basic::log('error', "Error [{$code}]: {$message} in {$file} on line {$line}");
    Basic::apiResponse(500, 'Internal Server Error');
    exit;
});

/**
 * Custom exception handler for the benchmark application
 * 
 * @param Exception $exception The exception to handle
 */
set_exception_handler(function ($exception) {
    Basic::log('error', "Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString());
    Basic::apiResponse(500, 'Internal Server Error');
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
 * 
 * @return void Returns empty response for benchmarking
 */
Basic::route('GET', '/', function () {
    Basic::log('debug', 'Root endpoint accessed');
    Basic::apiResponse(200, '', ['Content-Type' => 'text/plain']);
});

/**
 * Get user by ID
 * 
 * GET /user/{id}
 * 
 * @return void Returns user ID as plain text
 */
Basic::route('GET', '/user/(:num)', function () {
    $id = Basic::segment(2);
    Basic::log('debug', "User endpoint accessed with ID: {$id}");
    Basic::apiResponse(200, $id, ['Content-Type' => 'text/plain']);
});

/**
 * Create new user
 * 
 * POST /user
 * 
 * @return void Returns empty response for benchmarking
 */
Basic::route('POST', '/user', function () {
    Basic::log('debug', 'Create user endpoint accessed');
    Basic::apiResponse(200, '', ['Content-Type' => 'text/plain']);
});

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 * 
 * @return void Returns health status
 */
Basic::route('GET', '/health', function () {
    Basic::log('debug', 'Health check endpoint accessed');
    Basic::apiResponse(200, 'OK', ['Content-Type' => 'text/plain']);
});

// 404 handler
Basic::route('*', '*', function () {
    Basic::log('debug', '404 Not Found: ' . $_SERVER['REQUEST_URI']);
    Basic::apiResponse(404, 'Not Found', ['Content-Type' => 'text/plain']);
});

// Start the application
Basic::run();
