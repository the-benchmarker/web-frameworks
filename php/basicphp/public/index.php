<?php

/**
 * Production-grade BasicPHP Benchmark Server
 * 
 * A high-performance, production-ready benchmark server using BasicPHP framework.
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
define('APP_NAME', 'BasicPHP Benchmark Server');
define('APP_VERSION', '1.0.0');
define('DEBUG_MODE', false);

require_once __DIR__ . '/../Basic.php'; // BasicPHP class library

// ============================================================================
// SECURITY CONFIGURATION
// ============================================================================

// Security: Configure request body size limit for production
Basic::set('MAX_CONTENT_LENGTH', 16 * 1024 * 1024); // 16 MB

// Security: Set production logging level (only errors in production)
Basic::set('LOG_LEVEL', DEBUG_MODE ? 'debug' : 'error');
Basic::set('LOG_FORMAT', '[{timestamp}] {level} - {message}');

// ============================================================================
// SECURITY HEADERS MIDDLEWARE
// ============================================================================

// Add security headers to all responses
function addSecurityHeaders() {
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
 * Custom error handler for the benchmark application
 * Security: Don't expose internal error details
 * 
 * @param int $code Error code
 * @param string $message Error message
 * @param string $file File where error occurred
 * @param int $line Line number where error occurred
 */
set_error_handler(function ($code, $message, $file, $line) {
    if (DEBUG_MODE) {
        Basic::log('error', "Error [{$code}]: {$message} in {$file} on line {$line}");
    }
    addSecurityHeaders();
    Basic::apiResponse(500, 'Internal Server Error');
    exit;
});

/**
 * Custom exception handler for the benchmark application
 * Security: Don't expose internal error details
 * 
 * @param Exception $exception The exception to handle
 */
set_exception_handler(function ($exception) {
    if (DEBUG_MODE) {
        Basic::log('error', "Exception: " . $exception->getMessage() . "\n" . $exception->getTraceAsString());
    }
    addSecurityHeaders();
    Basic::apiResponse(500, 'Internal Server Error');
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
 * @return void Returns empty response for benchmarking
 */
Basic::route('GET', '/', function () {
    addSecurityHeaders();
    Basic::apiResponse(200, '', ['Content-Type' => 'text/plain']);
});

/**
 * Get user by ID
 * 
 * GET /user/{id}
 * 
 * @return void Returns user ID as plain text
 * Security: Validates input
 */
Basic::route('GET', '/user/(:num)', function () {
    addSecurityHeaders();
    $id = Basic::segment(2);
    
    // Input validation - security best practice
    if (empty($id) || !is_numeric($id)) {
        Basic::apiResponse(400, 'Bad Request: Missing or invalid ID parameter', ['Content-Type' => 'text/plain']);
        exit;
    }
    
    Basic::apiResponse(200, $id, ['Content-Type' => 'text/plain']);
});

/**
 * Create new user
 * 
 * POST /user
 * 
 * @return void Returns empty response with 201 Created status
 */
Basic::route('POST', '/user', function () {
    addSecurityHeaders();
    Basic::apiResponse(201, '', ['Content-Type' => 'text/plain']);
});

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 * 
 * @return void Returns health status
 */
Basic::route('GET', '/health', function () {
    addSecurityHeaders();
    Basic::apiResponse(200, 'OK', ['Content-Type' => 'text/plain']);
});

// 404 handler
Basic::route('*', '*', function () {
    addSecurityHeaders();
    Basic::apiResponse(404, 'Not Found', ['Content-Type' => 'text/plain']);
});

// ============================================================================
// STARTUP
// ============================================================================

// Start the application
Basic::run();
