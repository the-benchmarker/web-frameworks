<?php

/**
 * Slim Framework Benchmark Server
 * 
 * A high-performance benchmark server using Slim framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use Psr\Log\LoggerInterface;
use Slim\Factory\AppFactory;
use Slim\Logger;

require __DIR__ . '/../vendor/autoload.php';

/*
|--------------------------------------------------------------------------
| Application Setup
|--------------------------------------------------------------------------
*/

$app = AppFactory::create();

// Configure body parsing and limits
$app->addBodyParsingMiddleware();
$app->addRoutingMiddleware();
$app->addErrorMiddleware(true, true, true);

/*
|--------------------------------------------------------------------------
| Configuration
|--------------------------------------------------------------------------
*/

// Configure request body size limit (16 MB)
$app->getContainer()->set('settings', function () {
    return [
        'displayErrorDetails' => false,
        'logErrorDetails' => true,
        'logErrors' => true,
        'determineRouteBeforeAppMiddleware' => true,
        'upload_max_filesize' => '16M',
        'post_max_size' => '16M',
    ];
});

/*
|--------------------------------------------------------------------------
| Logging Setup
|--------------------------------------------------------------------------
*/

// Configure logger
$app->getContainer()->set(LoggerInterface::class, function () {
    $logger = new Logger([
        'name' => 'benchmark-slim',
        'path' => 'php://stdout',
        'level' => \Monolog\Logger::DEBUG,
    ]);
    
    $logger->pushHandler(new \Monolog\Handler\StreamHandler('php://stdout', \Monolog\Logger::DEBUG));
    return $logger;
});

/**
 * Get logger instance
 */
$logger = $app->getContainer()->get(LoggerInterface::class);

/*
|--------------------------------------------------------------------------
| Custom Error Handling
|--------------------------------------------------------------------------
*/

/**
 * Custom error handler
 */
$customErrorHandler = function (Request $request, \Throwable $exception, bool $displayErrorDetails) use ($logger) {
    $logger->error('Error: ' . $exception->getMessage() . '\n' . $exception->getTraceAsString());
    
    $response = $app->getResponseFactory()->createResponse();
    $response->getBody()->write('Internal Server Error');
    
    return $response->withStatus(500)->withHeader('Content-Type', 'text/plain');
};

$app->addErrorMiddleware(false, false, false, $customErrorHandler);

/**
 * PHP error handler
 */
set_error_handler(function ($code, $message, $file, $line) use ($logger) {
    $logger->error("Error [{$code}]: {$message} in {$file} on line {$line}");
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
});

/**
 * PHP exception handler
 */
set_exception_handler(function ($exception) use ($logger) {
    $logger->error("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString());
    http_response_code(500);
    header('Content-Type: text/plain');
    echo 'Internal Server Error';
    exit;
});

/*
|--------------------------------------------------------------------------
| Routes
|--------------------------------------------------------------------------
*/

/**
 * Root endpoint handler
 * 
 * GET /
 * 
 * @param Request $request HTTP request object
 * @param Response $response HTTP response object
 * @return Response Empty response for benchmarking
 */
$app->get('/', function (Request $request, Response $response) use ($logger): Response {
    $logger->debug('Root endpoint accessed');
    $response->getBody()->write('');
    return $response->withHeader('Content-Type', 'text/plain');
});

/**
 * Get user by ID
 * 
 * GET /user/{id}
 * 
 * @param Request $request HTTP request object
 * @param Response $response HTTP response object
 * @param array $args Route arguments
 * @return Response User ID as plain text
 */
$app->get('/user/{id}', function (Request $request, Response $response, array $args) use ($logger): Response {
    $id = $args['id'];
    $logger->debug("User endpoint accessed with ID: {$id}");
    $response->getBody()->write($id);
    return $response->withHeader('Content-Type', 'text/plain');
});

/**
 * Create new user
 * 
 * POST /user
 * 
 * @param Request $request HTTP request object
 * @param Response $response HTTP response object
 * @return Response Empty response for benchmarking
 */
$app->post('/user', function (Request $request, Response $response) use ($logger): Response {
    $logger->debug('Create user endpoint accessed');
    $response->getBody()->write('');
    return $response->withHeader('Content-Type', 'text/plain');
});

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 * 
 * @param Request $request HTTP request object
 * @param Response $response HTTP response object
 * @return Response Health status
 */
$app->get('/health', function (Request $request, Response $response) use ($logger): Response {
    $logger->debug('Health check endpoint accessed');
    $response->getBody()->write('OK');
    return $response->withHeader('Content-Type', 'text/plain');
});

$app->run();
