<?php

/**
 * BasicPHP - A frameworkless library-based approach for building web applications
 *          - and application programming interfaces or API's.
 *          - The aim of the project is for developers to build applications that are
 *          - framework-independent using vanilla PHP, and native functions and API's.
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

// // Register the start time/memory as a float value
// $time_start = microtime(TRUE);
// $memory_start = memory_get_usage();

// Load application
require_once '../app.php';

// // Register the end time/memory as a float value
// $time_end = microtime(TRUE);
// $memory_end = memory_get_usage();
// // Compute the elapsed time and memory
// $time_lapse = $time_end - $time_start;
// $memory_used = $memory_end - $memory_start;
// echo 'Lapse Time: ' . $time_lapse . ' seconds<br />';
// echo 'Memory Usage: ' . $memory_used . ' bytes<br />';

// // Compute average load speed. Set $_SESSION['speed'] as an array.
// if (! isset($_SESSION['speed'])) { $_SESSION['speed'] = []; }
// $_SESSION['speed'][] = $time_lapse;
// // Average load speed
// echo 'The average load speed is: ' . (array_sum($_SESSION['speed'])/count($_SESSION['speed']));
// var_dump($_SESSION['speed']);
// // Place a comment on session_destroy() to start computing average load speed.
// session_destroy();