<?php

declare(strict_types=1);

/**
 * Production-grade Antidot Framework Benchmark Server
 * 
 * A high-performance, production-ready benchmark server using Antidot framework.
 * Security best practices, performance optimizations, and clean code.
 * 
 * @author The Benchmarker Team
 * @version 1.0.0
 */

// ============================================================================
// PRODUCTION CONFIGURATION
// ============================================================================

// Security: Disable error display in production
ini_set('display_errors', '0');
// Security: Disable expose PHP version
ini_set('expose_php', '0');
// Performance: Only log errors, not warnings or notices
ini_set('log_errors', '1');
// Performance: Increase memory limit for production
ini_set('memory_limit', '256M');

// Production constants
define('APP_NAME', 'Antidot Benchmark Server');
define('APP_VERSION', '1.0.0');
define('DEBUG_MODE', false);

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

use Antidot\Framework\Application;
use Antidot\Runtime\AntidotRuntime;
use Psr\Container\ContainerInterface;

$_SERVER['APP_RUNTIME'] = AntidotRuntime::class;

chdir(dirname(__DIR__));
$rootDir = dirname(__DIR__);

// ============================================================================
// SECURITY HEADERS MIDDLEWARE
// ============================================================================

/**
 * Add security headers to response
 * Security: Prevent common web vulnerabilities
 */
function addSecurityHeaders(): void
{
    header('X-Content-Type-Options: nosniff');
    header('X-Frame-Options: DENY');
    header('X-XSS-Protection: 1; mode=block');
    header("Content-Security-Policy: default-src 'self'");
    header('Cache-Control: max-age=3600');
}

/*
|--------------------------------------------------------------------------
| Custom Error Handling
|--------------------------------------------------------------------------
*/

/**
 * Custom error handler for production
 * Security: Don't expose internal error details
 */
set_error_handler(function ($code, $message, $file, $line) {
    if (DEBUG_MODE) {
        error_log("[" . date('Y-m-d H:i:s') . "] ERROR - Error [{$code}]: {$message} in {$file} on line {$line}");
    }
    addSecurityHeaders();
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
});

/**
 * Custom exception handler for production
 * Security: Don't expose internal error details
 */
set_exception_handler(function ($exception) {
    if (DEBUG_MODE) {
        error_log("[" . date('Y-m-d H:i:s') . "] ERROR - Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString());
    }
    addSecurityHeaders();
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
});

require_once 'vendor/autoload_runtime.php';

return static function () use ($rootDir): ContainerInterface {
    addSecurityHeaders();
    $container = require $rootDir . '/config/container.php';
    $application = $container->get(Application::class);

    (require $rootDir . '/router/middleware.php')($application);
    (require $rootDir . '/router/routes.php')($application, $container);

    return $container;
};
