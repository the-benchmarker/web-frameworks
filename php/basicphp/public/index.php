<?php

/**
 * BasicPHP - A PHP Nano-Framework for Decoupled Application and Business Logic,
 *          - and Presentation. The aim of the project is for developers to build
 *          - applications that are framework-independent by decoupling the Model,
 *          - View and Controller from any framework, making the application portable
 *          - and compatible with the developer's framework of choice or plain PHP.
 *          -
 *          - To embed the application to any framework, copy BasicPHP's configuration
 *          - file (config.php), functions library (functions.php), and the 'classes',
 *          - 'models', 'views' and 'controllers' folders one (1) folder above the front
 *          - controller file of the chosen framework. In the controller file, at the
 *          - start of the script, include/require config.php and functions.php.
 *
 * @package  BasicPHP
 * @author   Raymund John Ang <raymund@open-nis.org>
 * @license  MIT License
 */

// Register the start time/memory as a float value
$time_start = microtime(true);
$memory_start = memory_get_usage();

// Bootstrap configuration
require_once '../config.php';

// Functions library
require_once '../functions.php';

// Routing configuration
require_once '../routes.php';

// // Register the end time/memory as a float value
// $time_end = microtime(TRUE);
// $memory_end = memory_get_usage();
// // Compute the elapsed time and memory
// $time_lapse = $time_end - $time_start;
// $memory_used = $memory_end - $memory_start;
// echo 'Lapse Time: ' . $time_lapse . ' seconds<br />';
// echo 'Memory Usage: ' . $memory_used . ' bytes<br />';
/*
// Compute average load speed. Set $_SESSION['speed'] as an array.
if (! isset($_SESSION['speed'])) { $_SESSION['speed'] = []; }
$_SESSION['speed'][] = $time_lapse;
// Average load speed
echo 'The average load speed is: ' . (array_sum($_SESSION['speed'])/count($_SESSION['speed']));
var_dump($_SESSION['speed']);
// Place a comment on session_destroy() to start computing average load speed.
session_destroy();
*/
