<?php

declare(strict_types=1);

/**
 * ChubbyPHP Framework Benchmark Server
 * 
 * A high-performance benchmark server using ChubbyPHP framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

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

/*
|--------------------------------------------------------------------------
| Custom Error Handling
|--------------------------------------------------------------------------
*/

/**
 * Custom error handler
 */
set_error_handler(function ($code, $message, $file, $line) {
    benchmark_log("Error [{$code}]: {$message} in {$file} on line {$line}", 'error');
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
});

/**
 * Custom exception handler
 */
set_exception_handler(function ($exception) {
    benchmark_log("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString(), 'error');
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
});

$loader = require __DIR__.'/../vendor/autoload.php';

$responseFactory = new ResponseFactory();

$app = new Application([
    new ExceptionMiddleware($responseFactory, true),
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
                $response = $this->responseFactory->createResponse();
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

$app->emit($app->handle((new ServerRequestFactory())->createFromGlobals()));
