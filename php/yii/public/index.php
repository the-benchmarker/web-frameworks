<?php

/**
 * Yii2 Framework Benchmark Server Entry Point
 * 
 * A high-performance benchmark server using Yii2 framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

// Configure for production/benchmarking environment
defined('YII_DEBUG') or define('YII_DEBUG', false);
defined('YII_ENV') or define('YII_ENV', 'prod');

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

require __DIR__ . '/../vendor/autoload.php';
require __DIR__ . '/../vendor/yiisoft/yii2/Yii.php';

/*
|--------------------------------------------------------------------------
| Custom Error Handling
|--------------------------------------------------------------------------
*/

/**
 * Custom error handler
 */
set_error_handler(function ($code, $message, $file, $line) {
    error_log("[" . date('Y-m-d H:i:s') . "] ERROR - Error [{$code}]: {$message} in {$file} on line {$line}");
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
});

/**
 * Custom exception handler
 */
set_exception_handler(function ($exception) {
    error_log("[" . date('Y-m-d H:i:s') . "] ERROR - Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString());
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
});

/*
|--------------------------------------------------------------------------
| Application Setup
|--------------------------------------------------------------------------
*/

$config = require __DIR__ . '/../config/web.php';

try {
    (new yii\web\Application($config))->run();
} catch (\Exception $e) {
    error_log("[" . date('Y-m-d H:i:s') . "] ERROR - Application Error: " . $e->getMessage() . "\n" . $e->getTraceAsString());
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
}
