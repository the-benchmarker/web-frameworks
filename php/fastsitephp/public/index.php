<?php

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

// -----------------------------------------------
// Create the setup the Application Object with
// Error Handling and UTC for the Timezone
// -----------------------------------------------

$app = new \FastSitePHP\Application();
$app->setup('UTC');

// -----------------------------------------------
// Define Routes
// -----------------------------------------------

$app->get('/', function() {
    return '';
});

$app->get('/user/:id', function($id) use ($app) {
    // Safely escape the user input since it's returned to the client.
    return $app->escape($id);
});

$app->post('/user', function() {
    return '';
});

// -------------------------
// Run the application
// -------------------------

$app->run();
