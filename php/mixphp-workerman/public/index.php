<?php

/**
 * MixPHP Workerman Framework Benchmark Server Entry Point
 * 
 * Production-grade benchmark server using MixPHP with Workerman.
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

require __DIR__ . '/../vendor/autoload.php';

// =============================================================================
// Security Headers
// =============================================================================

/**
 * Add security headers to response
 */
function addSecurityHeaders(array &$headers): void {
    $securityHeaders = [
        'X-Content-Type-Options' => 'nosniff',
        'X-Frame-Options' => 'DENY',
        'X-XSS-Protection' => '1; mode=block',
        'Content-Security-Policy' => "default-src 'self'",
        'Referrer-Policy' => 'strict-origin-when-cross-origin',
        'Cache-Control' => 'max-age=3600',
    ];
    
    foreach ($securityHeaders as $name => $value) {
        $headers[$name] = $value;
    }
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

$cpuCount = function () {
    if (strtolower(PHP_OS) === 'darwin') {
        $count = shell_exec('sysctl -n machdep.cpu.core_count');
    } else {
        $count = shell_exec('nproc');
    }
    $count = (int)$count > 0 ? (int)$count : 4;
    return $count > 4 ? $count - 2 : $count - 1;
};

$vega = App\Vega::new();
$http = new Workerman\Worker("http://0.0.0.0:3000");

if (\version_compare(\PHP_VERSION, '7.0.0', 'ge') // if php >= 7.0.0
    && \version_compare(php_uname('r'), '3.9', 'ge') // if kernel >=3.9
    && \strtolower(php_uname('s')) !== 'darwin' // if not Mac OS
) { // if not unix socket
    $http->reusePort = true;
}

$http->transport = 'tcp';
$http->name = 'MixPHP Workerman Benchmark Server';

$http->onMessage = function ($connection, $request) use ($vega) {
    try {
        $vega->handler()($connection, $request);
    } catch (\Throwable $e) {
        benchmark_log("Application Error: " . $e->getMessage() . "\n" . $e->getTraceAsString(), 'error');
        
        $headers = [
            'Content-Type' => 'text/plain',
            'X-Content-Type-Options' => 'nosniff',
            'X-Frame-Options' => 'DENY',
            'X-XSS-Protection' => '1; mode=block',
            'Content-Security-Policy' => "default-src 'self'",
        ];
        
        $body = DEBUG_MODE ? "Error: " . $e->getMessage() . "\nFile: " . $e->getFile() . ":" . $e->getLine() : 'Internal Server Error';
        $connection->send($body, 500, $headers);
    }
};

$http->count = $cpuCount();
Workerman\Worker::runAll();