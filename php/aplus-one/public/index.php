<?php

/**
 * Production-grade APlus One Framework Benchmark Server
 * 
 * A high-performance, production-ready benchmark server using APlus One framework.
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
define('APP_NAME', 'APlus One Benchmark Server');
define('APP_VERSION', '1.0.0');
define('DEBUG_MODE', false);

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

require __DIR__ . '/../vendor/autoload.php';

use Framework\MVC\App;
use Framework\Routing\RouteCollection;
use Framework\Routing\Router;

// ============================================================================
// SECURITY HEADERS MIDDLEWARE
// ============================================================================

/**
 * Add security headers to response
 * Security best practice: Add security headers to all responses
 */
function addSecurityHeaders(): void {
    header('X-Content-Type-Options: nosniff');
    header('X-Frame-Options: DENY');
    header('X-XSS-Protection: 1; mode=block');
    header("Content-Security-Policy: default-src 'self'");
    header('Cache-Control: max-age=3600');
}

/*
|--------------------------------------------------------------------------
| Logging Setup
|--------------------------------------------------------------------------
*/

/**
 * Custom logger for benchmarking
 * Production: Only log errors when not in debug mode
 * 
 * @param string $message Log message
 * @param string $level Log level (debug, info, error)
 */
function benchmark_log(string $message, string $level = 'debug'): void {
    if (DEBUG_MODE || $level === 'error') {
        $timestamp = date('Y-m-d H:i:s');
        error_log("[{$timestamp}] {$level} - {$message}");
    }
}

/*
|--------------------------------------------------------------------------
| Error Handling
|--------------------------------------------------------------------------
*/

/**
 * Custom error handler for production
 * Security: Don't expose internal error details
 */
set_error_handler(function ($code, $message, $file, $line) {
    benchmark_log("Error [{$code}]: {$message} in {$file} on line {$line}", 'error');
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
    benchmark_log("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString(), 'error');
    addSecurityHeaders();
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
});

try {
    addSecurityHeaders();
    (new App([
        'router' => [
            'default' => [
                'callback' => function (Router $router) {
                    $router->serve(null, function (RouteCollection $routes) {
                        /**
                         * Root endpoint handler
                         * GET /
                         */
                        $routes->get('/', function () {
                            addSecurityHeaders();
                            benchmark_log('Root endpoint accessed');
                            return '';
                        });
                        
                        /**
                         * Get user by ID
                         * GET /user/{int}
                         * Security: Validates input
                         */
                        $routes->get('/user/{int}', function ($args) {
                            addSecurityHeaders();
                            // Input validation - security best practice
                            if (empty($args[0])) {
                                http_response_code(400);
                                header('Content-Type: text/plain');
                                return 'Bad Request: Missing ID parameter';
                            }
                            benchmark_log("User endpoint accessed with ID: {$args[0]}");
                            return $args[0];
                        });
                        
                        /**
                         * Create new user
                         * POST /user
                         */
                        $routes->post('/user', function () {
                            addSecurityHeaders();
                            benchmark_log('Create user endpoint accessed');
                            http_response_code(201); // Created
                            return '';
                        });
                        
                        /**
                         * Health check endpoint for monitoring
                         * GET /health
                         */
                        $routes->get('/health', function () {
                            addSecurityHeaders();
                            benchmark_log('Health check endpoint accessed');
                            return 'OK';
                        });
                    });
                },
            ],
        ],
    ]))->runHttp();
} catch (\Exception $e) {
    benchmark_log("Application Error: " . $e->getMessage() . "\n" . $e->getTraceAsString(), 'error');
    addSecurityHeaders();
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
}
