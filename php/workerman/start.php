<?php

/**
 * Workerman Framework Benchmark Server
 * 
 * A high-performance benchmark server using Workerman framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

require_once __DIR__ . '/vendor/autoload.php';

use Workerman\Worker;

$worker = new Worker('http://0.0.0.0:3000');
$worker->count = shell_exec('nproc') ?: 32;

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
    // For Workerman, we can't exit, but we can log and continue
});

/**
 * Custom exception handler
 */
set_exception_handler(function ($exception) {
    benchmark_log("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString(), 'error');
    // For Workerman, we can't exit, but we can log and continue
});

$worker->onMessage = static function ($connection, $request) {
    $path = $request->path();
    
    // Log requests for benchmarking
    $clientIp = $connection->getRemoteIp();
    $method = $request->method();
    
    switch ($path) {
        case '/':
            benchmark_log("Root endpoint accessed from {$clientIp}");
            $connection->send('', ['Content-Type' => 'text/plain']);
            break;
            
        case '/user':
            benchmark_log("Create user endpoint accessed from {$clientIp}");
            $connection->send('', ['Content-Type' => 'text/plain']);
            break;
            
        case '/health':
            benchmark_log("Health check endpoint accessed from {$clientIp}");
            $connection->send('OK', ['Content-Type' => 'text/plain']);
            break;
            
        default:
            if (\str_starts_with($path, '/user/')) {
                $id = \substr($path, 6);
                benchmark_log("User endpoint accessed with ID: {$id} from {$clientIp}");
                $connection->send($id, ['Content-Type' => 'text/plain']);
            } else {
                benchmark_log("404 Not Found: {$path} from {$clientIp}", 'error');
                $connection->send('Not Found', 404, ['Content-Type' => 'text/plain']);
            }
            break;
    }
};

Worker::runAll();
