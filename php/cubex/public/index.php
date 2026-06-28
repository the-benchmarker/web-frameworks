<?php

/**
 * Cubex Framework Benchmark Server Entry Point
 * 
 * A high-performance benchmark server using Cubex framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

define('PHP_START', microtime(true));

use Cubex\Cubex;
use App\Application;

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

$loader = require_once(dirname(__DIR__) . '/vendor/autoload.php');

try {
    $cubex = new Cubex(dirname(__DIR__), $loader);
    $cubex->handle(new Application());
} catch(Throwable $e) {
    benchmark_log("Application Error: " . $e->getMessage() . "\n" . $e->getTraceAsString(), 'error');
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
} finally {
    if($cubex instanceof Cubex) {
        //Call the shutdown command
        $cubex->shutdown();
    }
}
