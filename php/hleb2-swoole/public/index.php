<?php

/**
 * HLEB2 Swoole Framework Benchmark Server Entry Point
 * 
 * Production-grade benchmark server using HLEB2 with Swoole.
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

// Performance settings
ini_set('memory_limit', '256M');

use Swoole\Http\Request;
use Swoole\Http\Response;
use Swoole\Http\Server;

include __DIR__ . "/../vendor/autoload.php";

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

$http = new Server('0.0.0.0', 3000, SWOOLE_BASE, SWOOLE_SOCK_TCP);
$http->set([
    'worker_num' => swoole_cpu_num() * 2,
    'enable_coroutine' => false,
    'log_file' => '/dev/null',
    'log_level' => SWOOLE_LOG_ERROR,
    'daemonize' => false,
    'max_request' => 10000,
]);

$config = [
    'common' => [
        'debug' => DEBUG_MODE,
        'allowed.hosts' => null,
        'log.enabled' => true,
        'max.log.level' => DEBUG_MODE ? 'debug' : 'error',
        'max.cli.log.level' => DEBUG_MODE ? 'debug' : 'error',
        'routes.auto-update' => false,
        'container.mock.allowed' => false,
        'app.cache.on' => false,
        'show.request.id' => false,
    ],
    'main' => [
        'session.enabled' => false,
        'session.cookie_httponly' => true,
        'session.cookie_secure' => true,
    ],
    'system' => [
        'classes.autoload' => true,
        'classes.preload' => false,
        'events.used' => false,
        'async.clear.state' => false,
    ],
];

$app = new Hleb\HlebAsyncBootstrap(__DIR__, $config);

$http->on('request', function ($request, Response $response) use ($app) {
    try {
        $result = $app->load($request)->getResponse();
        
        // Add security headers
        addSecurityHeaders($response);
        
        foreach ($result->getHeaders() as $name => $header) {
            $response->header($name, $header);
        }
        $response->status($result->getStatus(), (string)$result->getReason());
        $response->end($result->getBody());
    } catch (\Throwable $e) {
        benchmark_log("Application Error: " . $e->getMessage() . "\n" . $e->getTraceAsString(), 'error');
        addSecurityHeaders($response);
        $response->status(500);
        $response->header('Content-Type', 'text/plain');
        if (DEBUG_MODE) {
            $response->end("Error: " . $e->getMessage() . "\nFile: " . $e->getFile() . ":" . $e->getLine());
        } else {
            $response->end('Internal Server Error');
        }
    }
});

$http->start();
