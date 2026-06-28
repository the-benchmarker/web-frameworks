<?php

/**
 * Swoole Framework Benchmark Server
 * 
 * A high-performance benchmark server using Swoole framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

use Swoole\Http\Server;
use Swoole\Http\Request;
use Swoole\Http\Response;

$server = new Server('0.0.0.0', 3000, SWOOLE_BASE);
$server->set([
    'worker_num'       => swoole_cpu_num() * 2,
    'log_level'        => SWOOLE_LOG_ERROR,
    'log_file'         => '/dev/null',
    'enable_coroutine' => false,
]);

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

$server->on('request', function (Request $request, Response $response) {
    $uri = $request->server['request_uri'] ?? '/';
    
    // Log requests for benchmarking
    $clientIp = $request->server['remote_addr'] ?? 'unknown';
    $method = $request->server['request_method'] ?? 'GET';
    
    switch ($uri) {
        case '/':
            benchmark_log("Root endpoint accessed from {$clientIp}");
            $response->header('Content-Type', 'text/plain');
            $response->end('');
            break;
            
        case '/user':
            if ($method === 'POST') {
                benchmark_log("Create user endpoint accessed from {$clientIp}");
                $response->header('Content-Type', 'text/plain');
                $response->end('');
            } else {
                benchmark_log("404 Not Found: {$uri} from {$clientIp}", 'error');
                $response->header('Content-Type', 'text/plain');
                $response->status(404);
                $response->end('Not Found');
            }
            break;
            
        case '/health':
            benchmark_log("Health check endpoint accessed from {$clientIp}");
            $response->header('Content-Type', 'text/plain');
            $response->end('OK');
            break;
            
        default:
            if (strpos($uri, '/user/') === 0 && isset($uri[6])) {
                $id = substr($uri, 6);
                benchmark_log("User endpoint accessed with ID: {$id} from {$clientIp}");
                $response->header('Content-Type', 'text/plain');
                $response->end($id);
            } else {
                benchmark_log("404 Not Found: {$uri} from {$clientIp}", 'error');
                $response->header('Content-Type', 'text/plain');
                $response->status(404);
                $response->end('Not Found');
            }
            break;
    }
});

$server->start();
