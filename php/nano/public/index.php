<?php

/**
 * Nano Framework Benchmark Server
 * 
 * A high-performance benchmark server using Nano framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

use laylatichy\nano\core\request\Request;
use laylatichy\nano\core\response\Response;

require_once 'vendor/autoload.php';

// Configure application
useNano();

// Configure logging
useNano()->getContainer()->set('logger', function () {
    return new class {
        public function debug(string $message): void {
            error_log('[' . date('Y-m-d H:i:s') . '] DEBUG - ' . $message);
        }
        
        public function error(string $message): void {
            error_log('[' . date('Y-m-d H:i:s') . '] ERROR - ' . $message);
        }
    };
});

// Get logger instance
$logger = useNano()->getContainer()->get('logger');

// Configure request body limit
useNano()->getContainer()->set('body_limit', 16 * 1024 * 1024); // 16 MB

/*
|--------------------------------------------------------------------------
| Error Handling
|--------------------------------------------------------------------------
*/

/**
 * Custom error handler
 */
set_error_handler(function ($code, $message, $file, $line) use ($logger) {
    $logger->error("Error [{$code}]: {$message} in {$file} on line {$line}");
    useResponse()->withText('Internal Server Error')->withStatus(500)
        ->withHeader('Content-Type', 'text/plain')->send();
    exit;
});

/**
 * Custom exception handler
 */
set_exception_handler(function ($exception) use ($logger) {
    $logger->error("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString());
    useResponse()->withText('Internal Server Error')->withStatus(500)
        ->withHeader('Content-Type', 'text/plain')->send();
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
 * @param Request $request HTTP request object
 * @return Response Empty response for benchmarking
 */
useRouter()->get('/', fn (Request $request): Response => 
    useResponse()->withText('')->withHeader('Content-Type', 'text/plain')
);

/**
 * Get user by ID
 * 
 * GET /user/{id}
 * 
 * @param Request $request HTTP request object
 * @param string $id User identifier
 * @return Response User ID as plain text
 */
useRouter()->get('/user/{id}', function (Request $request, string $id) use ($logger): Response {
    $logger->debug("User endpoint accessed with ID: {$id}");
    return useResponse()->withText($id)->withHeader('Content-Type', 'text/plain');
});

/**
 * Create new user
 * 
 * POST /user
 * 
 * @param Request $request HTTP request object
 * @return Response Empty response for benchmarking
 */
useRouter()->post('/user', function (Request $request) use ($logger): Response {
    $logger->debug('Create user endpoint accessed');
    return useResponse()->withText('')->withHeader('Content-Type', 'text/plain');
});

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 * 
 * @param Request $request HTTP request object
 * @return Response Health status
 */
useRouter()->get('/health', function (Request $request) use ($logger): Response {
    $logger->debug('Health check endpoint accessed');
    return useResponse()->withText('OK')->withHeader('Content-Type', 'text/plain');
});

// Start the application
useNano()->start();
