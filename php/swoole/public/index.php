<?php

/**
 * Swoole Framework Benchmark Server
 * 
 * Production-grade benchmark server using Swoole framework.
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

use Swoole\Http\Server;
use Swoole\Http\Request;
use Swoole\Http\Response;

// =============================================================================
// Security Headers
// =============================================================================

/**
 * Add security headers to response
 */
function addSecurityHeaders(Response $response): void {
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

$server = new Server('0.0.0.0', 3000, SWOOLE_BASE);
$server->set([
    'worker_num' => swoole_cpu_num() * 2,
    'log_level' => SWOOLE_LOG_ERROR,
    'log_file' => '/dev/null',
    'enable_coroutine' => false,
    'daemonize' => false,
    'max_request' => 10000,
]);

$server->on('request', function (Request $request, Response $response) {
    try {
        addSecurityHeaders($response);
        
        $uri = $request->server['request_uri'] ?? '/';
        $clientIp = $request->server['remote_addr'] ?? 'unknown';
        $method = $request->server['request_method'] ?? 'GET';
        
        // Log requests for benchmarking
        benchmark_log("{$method} {$uri} from {$clientIp}");
        
        switch ($uri) {
            case '/':
                benchmark_log('Root endpoint accessed');
                $response->header('Content-Type', 'text/plain');
                $response->end('');
                break;
                
            case '/user':
                if ($method === 'POST') {
                    benchmark_log('Create user endpoint accessed');
                    $response->header('Content-Type', 'text/plain');
                    $response->status(201);
                    $response->end('');
                } else {
                    benchmark_log("404 Not Found: {$uri}", 'error');
                    $response->header('Content-Type', 'text/plain');
                    $response->status(404);
                    $response->end('Not Found');
                }
                break;
                
            case '/health':
                benchmark_log('Health check endpoint accessed');
                $response->header('Content-Type', 'text/plain');
                $response->end('OK');
                break;
                
            default:
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
                    
                    benchmark_log("User endpoint accessed with ID: {$id}");
                    $response->header('Content-Type', 'text/plain');
                    $response->end($id);
                } else {
                    benchmark_log("404 Not Found: {$uri}", 'error');
                    $response->header('Content-Type', 'text/plain');
                    $response->status(404);
                    $response->end('Not Found');
                }
                break;
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

$server->start();