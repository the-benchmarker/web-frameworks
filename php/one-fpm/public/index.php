<?php

/**
 * One FPM Framework Benchmark Server Entry Point
 * 
 * A high-performance benchmark server using One FPM framework.
 * Follows PHP best practices including proper error handling and logging.
 */

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

// Configure request body size limit (16 MB)
ini_set('post_max_size', '16M');
ini_set('upload_max_filesize', '16M');

require __DIR__.'/../App/index.php';
