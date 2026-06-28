<?php

/**
 * One FPM Framework Benchmark Server Application
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

define('_APP_PATH_', __DIR__);

define('_APP_PATH_VIEW_', __DIR__ . '/View');

function uuid()
{
    return '1';
}

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

require __DIR__ . '/../vendor/autoload.php';
require __DIR__ . '/../vendor/lizhichao/one/src/run.php';
require __DIR__ . '/config.php';

try {
    $req = new \One\Http\Request();
    $res = new \One\Http\Response($req);

    $router = new \One\Http\Router();
    list($req->class, $req->func, $mids, $action, $req->args, $req->as_name) = $router->explain($req->method(), $req->uri(), $req, $res);
    $f = $router->getExecAction($mids, $action, $res);
    echo $f();
} catch (\One\Exceptions\HttpException $e) {
    benchmark_log("HTTP Error: " . $e->getMessage(), 'error');
    echo \One\Exceptions\Handler::render($e);
} catch (Throwable $e) {
    benchmark_log("Exception: " . $e->getMessage() . "\n" . $e->getTraceAsString(), 'error');
    $msg = $e->getMessage();
    if ($e instanceof \One\Database\Mysql\DbException) {
        $msg = 'db error!';
    }
    echo \One\Exceptions\Handler::render(new \One\Exceptions\HttpException($res, $msg, $e->getCode()));
}
