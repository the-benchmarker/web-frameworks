<?php

declare(strict_types=1);

/**
 * Antidot Framework Benchmark Server Entry Point
 * 
 * A high-performance benchmark server using Antidot framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

use Antidot\Framework\Application;
use Antidot\Runtime\AntidotRuntime;
use Psr\Container\ContainerInterface;

ini_set('memory_limit', '2048M');

$_SERVER['APP_RUNTIME'] = AntidotRuntime::class;

chdir(dirname(__DIR__));
$rootDir = dirname(__DIR__);

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

require_once 'vendor/autoload_runtime.php';

return static function () use ($rootDir): ContainerInterface {
    $container = require $rootDir . '/config/container.php';
    $application = $container->get(Application::class);

    (require $rootDir . '/router/middleware.php')($application);
    (require $rootDir . '/router/routes.php')($application, $container);

    return $container;
};
