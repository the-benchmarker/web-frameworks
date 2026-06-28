<?php

declare(strict_types=1);

/**
 * Production-grade ChubbyPHP Framework Benchmark Server
 * 
 * A high-performance, production-ready benchmark server using ChubbyPHP framework.
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
// Performance: Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');
// Performance: Increase memory limit for production
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

// Production constants
define('APP_NAME', 'ChubbyPHP Benchmark Server');
define('APP_VERSION', '1.0.0');
define('DEBUG_MODE', false);

// ============================================================================
// PRODUCTION LOGGING
// ============================================================================

/**
 * Production-grade logger - only logs in debug mode
 * 
 * @param string $message Log message
 * @param string $level Log level (debug, info, error)
 */
function benchmark_log(string $message, string $level = 'debug'): void {
    if (DEBUG_MODE) {
        $timestamp = date('Y-m-d H:i:s');
        error_log("[{$timestamp}] {$level} - {$message}");
    }
}

// ============================================================================
// ERROR HANDLING
// ============================================================================

/**
 * Custom error handler for production
 * Security: Don't expose internal error details
 */
set_error_handler(function ($code, $message, $file, $line) {
    benchmark_log("Error [{$code}]: {$message} in {$file} on line {$line}", 'error');
    header('Content-Type: text/plain');
    http_response_code(500);
    echo 'Internal Server Error';
    exit;
});

/**
 * Custom exception handler for production
 * Security: Don't expose internal error details
 */
set_exception_handler(function ($exception) {
    benchmark_log("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString(), 'error');
    header('Content-Type: text/plain');
    http_response_code(500);
    echo 'Internal Server Error';
    exit;
});

$loader = require __DIR__.'/../vendor/autoload.php';

$responseFactory = new ResponseFactory();

// ============================================================================
// SECURITY HEADERS MIDDLEWARE
// ============================================================================

// Custom middleware to add security headers
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
            ->withHeader('Cache-Control', 'max-age=3600');
    }
}

$app = new Application([
    new ExceptionMiddleware($responseFactory, false), // Disable debug in production
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
                $response = $this->responseFactory->createResponse();
                return $response->withHeader('Content-Type', 'text/plain');
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
                $response = $this->responseFactory->createResponse(201); // Created
                return $response->withHeader('Content-Type', 'text/plain');
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
    ]), sys_get_temp_dir() . '/chubbyphp.php')),
]);

// ============================================================================
// STARTUP
// ============================================================================

$app->emit($app->handle((new ServerRequestFactory())->createFromGlobals()));
