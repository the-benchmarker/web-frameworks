<?php

/**
 * Slim Swoole Framework Benchmark Server Entry Point
 * 
 * Production-grade benchmark server using Slim with Swoole.
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

use Chubbyphp\SwooleRequestHandler\OnRequest;
use Chubbyphp\SwooleRequestHandler\PsrRequestFactory;
use Chubbyphp\SwooleRequestHandler\SwooleResponseEmitter;
use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use Slim\Factory\AppFactory;
use Slim\Psr7\Factory\ServerRequestFactory;
use Slim\Psr7\Factory\StreamFactory;
use Slim\Psr7\Factory\UploadedFileFactory;
use Swoole\Http\Server;

require __DIR__ . '/../vendor/autoload.php';

// =============================================================================
// Security Headers
// =============================================================================

/**
 * Add security headers to response
 */
function addSecurityHeaders(Response $response): Response {
    return $response
        ->withHeader('X-Content-Type-Options', 'nosniff')
        ->withHeader('X-Frame-Options', 'DENY')
        ->withHeader('X-XSS-Protection', '1; mode=block')
        ->withHeader('Content-Security-Policy', "default-src 'self'")
        ->withHeader('Referrer-Policy', 'strict-origin-when-cross-origin')
        ->withHeader('Cache-Control', 'max-age=3600');
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

$app = AppFactory::create();

// Root endpoint
$app->get('/', function (Request $request, Response $response): Response {
    benchmark_log('Root endpoint accessed');
    return addSecurityHeaders($response->withHeader('Content-Type', 'text/plain'));
});

// Get user by ID
$app->get('/user/{id}', function (Request $request, Response $response, array $args): Response {
    $id = $args['id'] ?? '';
    benchmark_log("User endpoint accessed with ID: {$id}");
    
    // Input validation - security best practice
    if (empty($id)) {
        return addSecurityHeaders($response->withStatus(400)
            ->withHeader('Content-Type', 'text/plain'))
            ->withBody("Bad Request: Missing ID parameter");
    }
    
    return addSecurityHeaders($response->withHeader('Content-Type', 'text/plain'))
        ->withBody($id);
});

// Create new user
$app->post('/user', function (Request $request, Response $response): Response {
    benchmark_log('Create user endpoint accessed');
    return addSecurityHeaders($response->withStatus(201)
        ->withHeader('Content-Type', 'text/plain'));
});

// Health check endpoint
$app->get('/health', function (Request $request, Response $response): Response {
    benchmark_log('Health check endpoint accessed');
    return addSecurityHeaders($response->withHeader('Content-Type', 'text/plain'))
        ->withBody('OK');
});

$server = new Server('0.0.0.0', 3000);

$server->set([
    'worker_num' => swoole_cpu_num() * 2,
    'enable_coroutine' => false,
    'log_file' => '/dev/null',
    'log_level' => SWOOLE_LOG_ERROR,
    'daemonize' => false,
    'max_request' => 10000,
]);

$server->on('request', new OnRequest(
    new PsrRequestFactory(
        new ServerRequestFactory(),
        new StreamFactory(),
        new UploadedFileFactory()
    ),
    new SwooleResponseEmitter(),
    $app
));

$server->start();