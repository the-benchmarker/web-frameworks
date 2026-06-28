<?php

/**
 * HLEB2 RoadRunner Framework Benchmark Server Entry Point
 * 
 * Production-grade benchmark server using HLEB2 with RoadRunner.
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

use Spiral\RoadRunner;
use Nyholm\Psr7;

include __DIR__ . "/../vendor/autoload.php";

// =============================================================================
// Security Headers
// =============================================================================

/**
 * Add security headers to response
 */
function addSecurityHeaders(): array {
    return [
        'X-Content-Type-Options' => 'nosniff',
        'X-Frame-Options' => 'DENY',
        'X-XSS-Protection' => '1; mode=block',
        'Content-Security-Policy' => "default-src 'self'",
        'Referrer-Policy' => 'strict-origin-when-cross-origin',
        'Cache-Control' => 'max-age=3600',
    ];
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

$worker = RoadRunner\Worker::create();
$psrFactory = new Psr7\Factory\Psr17Factory();

$psr7 = new RoadRunner\Http\PSR7Worker($worker, $psrFactory, $psrFactory, $psrFactory);

$app = new Hleb\HlebAsyncBootstrap(__DIR__, $config);

while ($request = $psr7->waitRequest()) {
    try {
        $response = $app->load($request)->getResponse();
        
        // Add security headers
        $securityHeaders = addSecurityHeaders();
        foreach ($securityHeaders as $name => $value) {
            $response['headers'][$name] = [$value];
        }
        
        $psr7->respond(new Psr7\Response(...$response->getArgs()));
    } catch (\Throwable $e) {
        benchmark_log("Application Error: " . $e->getMessage() . "\n" . $e->getTraceAsString(), 'error');
        $app->errorLog($e);
    }
}
