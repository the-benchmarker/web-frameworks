<?php

/**
 * Swoole Coroutine Framework Benchmark Server
 * 
 * Production-grade benchmark server using Swoole Coroutine framework.
 * Implements security best practices, proper error handling, and optimized logging.
 */

// =============================================================================
// PRODUCTION CONFIGURATION
// =============================================================================

error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('display_startup_errors', '0');
ini_set('log_errors', '1');
ini_set('log_errors_max_len', '1024');
ini_set('ignore_repeated_errors', '1');
ini_set('ignore_repeated_source', '1');
ini_set('html_errors', '0');

define('DEBUG_MODE', false);

// Security settings
ini_set('expose_php', '0');
ini_set('session.cookie_httponly', '1');
ini_set('session.cookie_secure', '1');

// Performance settings
ini_set('memory_limit', '256M');

use Swoole\Process\Pool;
use Swoole\Coroutine\Http\Server;

// =============================================================================
// Security Headers
// =============================================================================

/**
 * Add security headers to response
 */
function addSecurityHeaders($response): void {
    $response->header('X-Content-Type-Options', 'nosniff');
    $response->header('X-Frame-Options', 'DENY');
    $response->header('X-XSS-Protection', '1; mode=block');
    $response->header('Content-Security-Policy', "default-src 'self'");
    $response->header('Referrer-Policy', 'strict-origin-when-cross-origin');
    $response->header('Cache-Control', 'max-age=3600');
}

/*
|--------------------------------------------------------------------------
| Logging Setup
|--------------------------------------------------------------------------
*/

/**
 * Production-grade logger for benchmarking
 * 
 * @param string $message Log message
 * @param string $level Log level (debug, info, warning, error, critical)
 */
function benchmark_log(string $message, string $level = 'debug'): void {
    if (!DEBUG_MODE && $level === 'debug') {
        return;
    }
    
    $timestamp = date('Y-m-d H:i:s');
    $logEntry = sprintf("[%s] %s - %s", $timestamp, strtoupper($level), $message);
    
    // Log to error log
    error_log($logEntry);
    
    // In production, don't log debug messages to slow down the application
    if (DEBUG_MODE) {
        if ($level === 'error' || $level === 'critical') {
            fwrite(STDERR, $logEntry . PHP_EOL);
        }
    }
}

$worker_num = swoole_cpu_num() * 2;

/**
 * The Coroutine\Http\Server does not automatically create multiple processes,
 * needs to be used with the Process\Pool module to take advantage of multiple cores.
 */
$pool = new Pool($worker_num);
$pool->set(['enable_coroutine' => true]);
$pool->on('workerStart', function ($pool, $id) {
    $server = new Server('0.0.0.0', 3000, false, true);
    
    $server->handle('/', function ($request, $response) {
        try {
            addSecurityHeaders($response);
            $clientIp = $request->server['remote_addr'] ?? 'unknown';
            benchmark_log("Root endpoint accessed from {$clientIp}");
            $response->header('Content-Type', 'text/plain');
            $response->end('');
        } catch (\Throwable $e) {
            benchmark_log("Application Error: " . $e->getMessage() . "\n" . $e->getTraceAsString(), 'error');
            addSecurityHeaders($response);
            $response->header('Content-Type', 'text/plain');
            $response->status(500);
            if (DEBUG_MODE) {
                $response->end("Error: " . $e->getMessage() . "\nFile: " . $e->getFile() . ":" . $e->getLine());
            } else {
                $response->end('Internal Server Error');
            }
        }
    });
    
    $server->handle('/user', function ($request, $response) {
        try {
            addSecurityHeaders($response);
            $clientIp = $request->server['remote_addr'] ?? 'unknown';
            $uri = $request->server['request_uri'] ?? '/user';
            $method = $request->server['request_method'] ?? 'GET';
            
            if (strpos($uri, '/user/') === 0 && isset($uri[6])) {
                $id = substr($uri, 6);
                
                // Input validation - security best practice
                if (empty($id)) {
                    benchmark_log('Bad Request: Missing ID parameter', 'warning');
                    $response->header('Content-Type', 'text/plain');
                    $response->status(400);
                    $response->end('Bad Request: Missing ID parameter');
                    return;
                }
                
                benchmark_log("User endpoint accessed with ID: {$id} from {$clientIp}");
                $response->header('Content-Type', 'text/plain');
                $response->end($id);
            } else {
                if ($method === 'POST') {
                    benchmark_log("Create user endpoint accessed from {$clientIp}");
                    $response->header('Content-Type', 'text/plain');
                    $response->status(201);
                    $response->end('');
                } else {
                    benchmark_log("404 Not Found: /user", 'error');
                    $response->header('Content-Type', 'text/plain');
                    $response->status(404);
                    $response->end('Not Found');
                }
            }
        } catch (\Throwable $e) {
            benchmark_log("Application Error: " . $e->getMessage() . "\n" . $e->getTraceAsString(), 'error');
            addSecurityHeaders($response);
            $response->header('Content-Type', 'text/plain');
            $response->status(500);
            if (DEBUG_MODE) {
                $response->end("Error: " . $e->getMessage() . "\nFile: " . $e->getFile() . ":" . $e->getLine());
            } else {
                $response->end('Internal Server Error');
            }
        }
    });
    
    $server->handle('/health', function ($request, $response) {
        try {
            addSecurityHeaders($response);
            $clientIp = $request->server['remote_addr'] ?? 'unknown';
            benchmark_log("Health check endpoint accessed from {$clientIp}");
            $response->header('Content-Type', 'text/plain');
            $response->end('OK');
        } catch (\Throwable $e) {
            benchmark_log("Application Error: " . $e->getMessage() . "\n" . $e->getTraceAsString(), 'error');
            addSecurityHeaders($response);
            $response->header('Content-Type', 'text/plain');
            $response->status(500);
            if (DEBUG_MODE) {
                $response->end("Error: " . $e->getMessage() . "\nFile: " . $e->getFile() . ":" . $e->getLine());
            } else {
                $response->end('Internal Server Error');
            }
        }
    });
    
    $server->start();
});
$pool->start();