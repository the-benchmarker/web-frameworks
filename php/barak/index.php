<?php

// composer packages load
require "vendor/autoload.php";

// load application
require_once "lib/Application.php";

$time_start = microtime(true);

// kick application
Application::run();

$time_end = microtime(true);

$time = $time_end - $time_start;
$worked_at = strftime('%F %T');

// ApplicationLogger::info("{$_SERVER['REQUEST_METHOD']} {$_SERVER['REQUEST_URI']} at {$worked_at} agent {$_SERVER['HTTP_USER_AGENT']} in {$time}");
?>