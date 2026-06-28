<?php

declare(strict_types=1);

/**
 * ChubbyPHP RoadRunner Framework Benchmark Server Entry Point
 * 
 * Production-grade benchmark server using ChubbyPHP with RoadRunner.
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

define('APP_NAME', 'ChubbyPHP RoadRunner Benchmark Server');

// Security settings
ini_set('expose_php', '0');

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

// Performance settings
ini_set('memory_limit', '256M');

namespace App;

use Chubbyphp\Framework\Application;
use Chubbyphp\Framework\Middleware\ExceptionMiddleware;
use Chubbyphp\Framework\Middleware\RouteMatcherMiddleware;
use Chubbyphp\Framework\Router\FastRoute\RouteMatcher;
use Chubbyphp\Framework\Router\Route;
use Chubbyphp\Framework\Router\RoutesByName;
use Psr\Http\Message\ResponseFactoryInterface;
use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Psr\Http\Server\RequestHandlerInterface;
use Slim\Psr7\Factory\ResponseFactory;
use Slim\Psr7\Factory\ServerRequestFactory;
use Slim\Psr7\Factory\StreamFactory;
use Slim\Psr7\Factory\UploadedFileFactory;
use Spiral\RoadRunner\Http\PSR7Worker;
use Spiral\RoadRunner\Worker;

// =============================================================================
// Security Headers Middleware
// =============================================================================

/**
 * Custom middleware to add security headers
 */
class SecurityHeadersMiddleware implements \Psr\Http\Server\MiddlewareInterface
{
    public function process(ServerRequestInterface $request, RequestHandlerInterface $handler): ResponseInterface
    {
        $response = $handler->handle($request);
        return $response
            ->withHeader('X-Content-Type-Options', 'nosniff')
            ->withHeader('X-Frame-Options', 'DENY')
            ->withHeader('X-XSS-Protection', '1; mode=block')
            ->withHeader('Content-Security-Policy', "default-src 'self'")
            ->withHeader('Referrer-Policy', 'strict-origin-when-cross-origin')
            ->withHeader('Cache-Control', 'max-age=3600');
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

/*
|--------------------------------------------------------------------------
| Error Handling
|--------------------------------------------------------------------------
*/

/**
 * Production error handler
 */
set_error_handler(function ($code, $message, $file, $line) {
    if (!DEBUG_MODE) {
        benchmark_log("Error [{$code}]: {$message} in {$file} on line {$line}", 'error');
        http_response_code(500);
        header('Content-Type: text/plain');
        echo 'Internal Server Error';
        exit;
    }
});

/**
 * Production exception handler
 */
set_exception_handler(function ($exception) {
    benchmark_log("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString(), 'error');
    http_response_code(500);
    header('Content-Type: text/plain');
    if (DEBUG_MODE) {
        echo "Error: " . $exception->getMessage() . "\nFile: " . $exception->getFile() . ":" . $exception->getLine();
    } else {
        echo 'Internal Server Error';
    }
    exit;
});

$loader = require __DIR__.'/../vendor/autoload.php';

$responseFactory = new ResponseFactory();

$app = new Application([
    new ExceptionMiddleware($responseFactory, DEBUG_MODE),
    new SecurityHeadersMiddleware(),
    new RouteMatcherMiddleware(new RouteMatcher(new RoutesByName([
        // Root endpoint
        Route::get('/', 'home', new class ($responseFactory) implements RequestHandlerInterface {
            public function __construct(private ResponseFactoryInterface $responseFactory)
            {
            }
            public function handle(ServerRequestInterface $request): ResponseInterface
            {
                benchmark_log('Root endpoint accessed');
                return $this->responseFactory->createResponse(200)->withHeader('Content-Type', 'text/plain');
            }
        }),
        
        // Get user by ID
        Route::get('/user/{id}', 'user_view', new class ($responseFactory) implements RequestHandlerInterface {
            public function __construct(private ResponseFactoryInterface $responseFactory)
            {
            }
            public function handle(ServerRequestInterface $request): ResponseInterface
            {
                $id = $request->getAttribute('id');
                benchmark_log("User endpoint accessed with ID: {$id}");
                
                // Input validation - security best practice
                if (empty($id)) {
                    $response = $this->responseFactory->createResponse(400);
                    $response->getBody()->write('Bad Request: Missing ID parameter');
                    return $response->withHeader('Content-Type', 'text/plain');
                }
                
                $response = $this->responseFactory->createResponse();
                $response->getBody()->write($id);
                return $response->withHeader('Content-Type', 'text/plain');
            }
        }),
        
        // Create new user
        Route::post('/user', 'user_list', new class ($responseFactory) implements RequestHandlerInterface {
            public function __construct(private ResponseFactoryInterface $responseFactory)
            {
            }
            public function handle(ServerRequestInterface $request): ResponseInterface
            {
                benchmark_log('Create user endpoint accessed');
                return $this->responseFactory->createResponse(201)->withHeader('Content-Type', 'text/plain');
            }
        }),
        
        // Health check endpoint
        Route::get('/health', 'health_check', new class ($responseFactory) implements RequestHandlerInterface {
            public function __construct(private ResponseFactoryInterface $responseFactory)
            {
            }
            public function handle(ServerRequestInterface $request): ResponseInterface
            {
                benchmark_log('Health check endpoint accessed');
                $response = $this->responseFactory->createResponse();
                $response->getBody()->write('OK');
                return $response->withHeader('Content-Type', 'text/plain');
            }
        }),
    ]), sys_get_temp_dir() . '/chubbyphp-roadrunner.php')),
]);

$worker = new PSR7Worker(
    Worker::create(),
    new ServerRequestFactory(),
    new StreamFactory(),
    new UploadedFileFactory()
);

while ($req = $worker->waitRequest()) {
    try {
        $worker->respond($app->handle($req));
    } catch (\Throwable $e) {
        benchmark_log("Application Error: " . $e->getMessage() . "\n" . $e->getTraceAsString(), 'error');
        $worker->getWorker()->error((string)$e);
    }
}
