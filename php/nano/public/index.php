<?php

/**
 * Production-grade Nano Framework Benchmark Server
 * 
 * A high-performance, production-ready benchmark server using Nano framework.
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

use laylatichy\nano\core\request\Request;
use laylatichy\nano\core\response\Response;

require_once 'vendor/autoload.php';

// Production constants
define('APP_NAME', 'Nano Benchmark Server');
define('APP_VERSION', '1.0.0');
define('DEBUG_MODE', false);

// ============================================================================
// APPLICATION SETUP
// ============================================================================

// Configure application
useNano();

// Configure request body limit for production
useNano()->getContainer()->set('body_limit', 16 * 1024 * 1024); // 16 MB

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

// ============================================================================
// PRODUCTION LOGGING
// ============================================================================

// Configure logger for production (only errors)
useNano()->getContainer()->set('logger', function () {
    return new class {
        public function debug(string $message): void {
            if (DEBUG_MODE) {
                error_log('[' . date('Y-m-d H:i:s') . '] DEBUG - ' . $message);
            }
        }
        
        public function error(string $message): void {
            error_log('[' . date('Y-m-d H:i:s') . '] ERROR - ' . $message);
        }
    };
});

// Get logger instance
$logger = useNano()->getContainer()->get('logger');

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
    header('Content-Security-Policy: default-src \'self\'');
    header('Cache-Control: max-age=3600');
}

// ============================================================================
// ERROR HANDLING
// ============================================================================

/**
 * Custom error handler for production
 * Security: Don't expose internal error details
 */
set_error_handler(function ($code, $message, $file, $line) use ($logger) {
    $logger->error("Error [{$code}]: {$message} in {$file} on line {$line}");
    addSecurityHeaders();
    useResponse()->withText('Internal Server Error')->withStatus(500)
        ->withHeader('Content-Type', 'text/plain')->send();
    exit;
});

/**
 * Custom exception handler for production
 * Security: Don't expose internal error details
 */
set_exception_handler(function ($exception) use ($logger) {
    $logger->error("Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString());
    addSecurityHeaders();
    useResponse()->withText('Internal Server Error')->withStatus(500)
        ->withHeader('Content-Type', 'text/plain')->send();
    exit;
});

// ============================================================================
// ROUTES
// ============================================================================

/**
 * Root endpoint handler
 * 
 * GET /
 * 
 * @param Request $request HTTP request object
 * @return Response Empty response for benchmarking
 */
useRouter()->get('/', function (Request $request): Response {
    addSecurityHeaders();
    return useResponse()->withText('')->withHeader('Content-Type', 'text/plain');
});

/**
 * Get user by ID
 * 
 * GET /user/{id}
 * 
 * @param Request $request HTTP request object
 * @param string $id User identifier
 * @return Response User ID as plain text
 * Security: Validates input
 */
useRouter()->get('/user/{id}', function (Request $request, string $id) use ($logger): Response {
    addSecurityHeaders();
    
    // Input validation - security best practice
    if (empty($id)) {
        useResponse()->withText('Bad Request: Missing ID parameter')->withStatus(400)
            ->withHeader('Content-Type', 'text/plain')->send();
        exit;
    }
    
    $logger->debug("User endpoint accessed with ID: {$id}");
    return useResponse()->withText($id)->withHeader('Content-Type', 'text/plain');
});

/**
 * Create new user
 * 
 * POST /user
 * 
 * @param Request $request HTTP request object
 * @return Response Empty response with 201 Created status
 */
useRouter()->post('/user', function (Request $request) use ($logger): Response {
    addSecurityHeaders();
    $logger->debug('Create user endpoint accessed');
    return useResponse()->withText('')->withStatus(201)->withHeader('Content-Type', 'text/plain'); // Created
});

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 * 
 * @param Request $request HTTP request object
 * @return Response Health status
 */
useRouter()->get('/health', function (Request $request) use ($logger): Response {
    addSecurityHeaders();
    $logger->debug('Health check endpoint accessed');
    return useResponse()->withText('OK')->withHeader('Content-Type', 'text/plain');
});

// 404 handler
useRouter()->add('*', '/{path:.*}', function (Request $request): Response {
    addSecurityHeaders();
    return useResponse()->withText('Not Found')->withStatus(404)->withHeader('Content-Type', 'text/plain');
});

// ============================================================================
// STARTUP
// ============================================================================

// Start the application
useNano()->start();
