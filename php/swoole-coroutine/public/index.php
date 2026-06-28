<?php

/**
 * Swoole Coroutine Framework Benchmark Server
 * 
 * A high-performance benchmark server using Swoole Coroutine framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

use Swoole\Process\Pool;
use Swoole\Coroutine\Http\Server;

$worker_num = swoole_cpu_num() * 2;

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

/**
 * The Coroutine\Http\Server does not automatically create multiple processes,
 * needs to be used with the Process\Pool module to take advantage of multiple cores.
 */
$pool = new Pool($worker_num);
$pool->set(['enable_coroutine' => true]);
$pool->on('workerStart', function ($pool, $id) {
    $server = new Server('0.0.0.0', 3000, false, true);
    
    $server->handle('/', function ($request, $response) {
        $clientIp = $request->server['remote_addr'] ?? 'unknown';
        benchmark_log("Root endpoint accessed from {$clientIp}");
        $response->header('Content-Type', 'text/plain');
        $response->end('');
    });
    
    $server->handle('/user', function ($request, $response) {
        $clientIp = $request->server['remote_addr'] ?? 'unknown';
        $uri = $request->server['request_uri'] ?? '/user';
        
        if (strpos($uri, '/user/') === 0 && isset($uri[6])) {
            $id = substr($uri, 6);
            benchmark_log("User endpoint accessed with ID: {$id} from {$clientIp}");
            $response->header('Content-Type', 'text/plain');
            $response->end($id);
        } else {
            benchmark_log("Create user endpoint accessed from {$clientIp}");
            $response->header('Content-Type', 'text/plain');
            $response->end('');
        }
    });
    
    $server->handle('/health', function ($request, $response) {
        $clientIp = $request->server['remote_addr'] ?? 'unknown';
        benchmark_log("Health check endpoint accessed from {$clientIp}");
        $response->header('Content-Type', 'text/plain');
        $response->end('OK');
    });
    
    $server->start();
});
$pool->start();
