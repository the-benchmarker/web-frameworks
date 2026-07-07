#!/usr/bin/env php
<?php

// =============================================================================
// PRODUCTION CONFIGURATION
// =============================================================================

ini_set('display_errors', '0');
ini_set('display_startup_errors', '0');
ini_set('log_errors', '1');
ini_set('log_errors_max_len', '1024');
ini_set('ignore_repeated_errors', '1');
ini_set('ignore_repeated_source', '1');
ini_set('html_errors', '0');

define('DEBUG_MODE', false);

error_reporting(E_ALL);
date_default_timezone_set('UTC');

// Security settings
ini_set('expose_php', '0');

! defined('BASE_PATH') && define('BASE_PATH', dirname(__DIR__, 1));
! defined('SWOOLE_HOOK_FLAGS') && define('SWOOLE_HOOK_FLAGS', SWOOLE_HOOK_ALL);

require BASE_PATH . '/vendor/autoload.php';

// =============================================================================
// Security Headers and Error Handling
// =============================================================================

/**
 * Add security headers to response
 */
function addSecurityHeaders(): void {
    header('X-Content-Type-Options: nosniff');
    header('X-Frame-Options: DENY');
    header('X-XSS-Protection: 1; mode=block');
    header('Content-Security-Policy: default-src \'self\'');
    header('Referrer-Policy: strict-origin-when-cross-origin');
}

/**
 * Production-grade logger
 */
function benchmark_log(string $message, string $level = 'info'): void {
    if (!DEBUG_MODE && $level === 'debug') {
        return;
    }
    
    $timestamp = date('Y-m-d H:i:s');
    $logEntry = sprintf("[%s] %s - %s", $timestamp, strtoupper($level), $message);
    error_log($logEntry);
    
    if (DEBUG_MODE && ($level === 'error' || $level === 'critical')) {
        fwrite(STDERR, $logEntry . PHP_EOL);
    }
}

/**
 * Production error handler
 */
function productionErrorHandler(int $errno, string $errstr, string $errfile = null, int $errline = null): bool {
    $errorLevels = [E_ERROR, E_PARSE, E_CORE_ERROR, E_COMPILE_ERROR, E_USER_ERROR];
    
    if (in_array($errno, $errorLevels, true)) {
        benchmark_log("Fatal Error: {$errstr} in {$errfile}:{$errline}", 'critical');
        if (!DEBUG_MODE) {
            http_response_code(500);
            header('Content-Type: text/plain');
            echo 'Internal Server Error';
            exit(1);
        }
    }
    
    if (!DEBUG_MODE && $errno !== E_NOTICE && $errno !== E_DEPRECATED) {
        benchmark_log("Warning: {$errstr} in {$errfile}:{$errline}", 'warning');
    }
    
    return false;
}

/**
 * Production exception handler
 */
function productionExceptionHandler(Throwable $e): void {
    addSecurityHeaders();
    benchmark_log("Application Error: " . $e->getMessage() . "\nStack Trace: " . $e->getTraceAsString(), 'error');
    
    if (DEBUG_MODE) {
        http_response_code(500);
        header('Content-Type: text/plain');
        echo "Error: " . $e->getMessage() . "\nFile: " . $e->getFile() . ":" . $e->getLine();
    } else {
        http_response_code(500);
        header('Content-Type: text/plain');
        echo 'Internal Server Error';
    }
}

// Set error handlers
set_error_handler('productionErrorHandler');
set_exception_handler('productionExceptionHandler');
register_shutdown_function(function(): void {
    $error = error_get_last();
    if ($error !== null && $error['type'] === E_ERROR) {
        productionErrorHandler($error['type'], $error['message'], $error['file'] ?? 'unknown', $error['line'] ?? 0);
    }
});

// Self-called anonymous function that creates its own scope and keep the global namespace clean.
(function () {
    try {
        Hyperf\Di\ClassLoader::init();
        /** @var \Psr\Container\ContainerInterface $container */
        $container = require BASE_PATH . '/config/container.php';

        $application = $container->get(\Hyperf\Contract\ApplicationInterface::class);
        $application->run();
    } catch (Throwable $e) {
        productionExceptionHandler($e);
        exit(1);
    }
})();
