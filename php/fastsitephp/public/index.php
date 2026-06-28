<?php

/**
 * FastSitePHP Framework Benchmark Server
 * 
 * A high-performance benchmark server using FastSitePHP framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

// -----------------------------------------------
// Load Dependencies (additional PHP files)
// -----------------------------------------------

// Setup a PHP Autoloader
// This allows classes to be dynamically loaded and is included when
// dependencies are installed through the PHP Package Manager Composer.
//
// require __DIR__ . '/../vendor/autoload.php';

// Or for a minimal site when using FastSitePHP only the following 2 files
// need to be included.
//
// Using location based on install from:
//     composer require fastsitephp/fastsitephp
//
// This also assumes that this file is located under a [public]
// directory (or directory with another name and same dir structure)
//
require __DIR__ . '/../vendor/fastsitephp/fastsitephp/src/Application.php';
require __DIR__ . '/../vendor/fastsitephp/fastsitephp/src/Route.php';

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

// -----------------------------------------------
// Create the setup the Application Object with
// Error Handling and UTC for the Timezone
// -----------------------------------------------

$app = new \FastSitePHP\Application();
$app->setup('UTC');

// -----------------------------------------------
// Define Routes
// -----------------------------------------------

/**
 * Root endpoint handler
 * 
 * GET /
 */
$app->get('/', function () {
    benchmark_log('Root endpoint accessed');
    return '';
});

/**
 * Get user by ID
 * 
 * GET /user/{id}
 * 
 * @param string $id User identifier
 */
$app->get('/user/:id', function ($id) use ($app) {
    benchmark_log("User endpoint accessed with ID: {$id}");
    // Safely escape the user input since it's returned to the client.
    return $app->escape($id);
});

/**
 * Create new user
 * 
 * POST /user
 */
$app->post('/user', function () {
    benchmark_log('Create user endpoint accessed');
    return '';
});

/**
 * Health check endpoint for monitoring
 * 
 * GET /health
 */
$app->get('/health', function () {
    benchmark_log('Health check endpoint accessed');
    return 'OK';
});

// -------------------------
// Run the application
// -------------------------

$app->run();
