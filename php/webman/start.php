<?php
require_once __DIR__ . '/vendor/autoload.php';

use Workerman\Worker;
use Workerman\Protocols\Http;
use Workerman\Connection\TcpConnection;
use Webman\App;
use Webman\Config;
use Webman\Route;
use Webman\Middleware;
use support\Request;
use support\bootstrap\Log;
use support\bootstrap\Container;


Config::load(config_path(), ['route', 'container']);
$config = config('server');

if ($timezone = config('app.default_timezone')) {
    date_default_timezone_set($timezone);
}

if (!is_dir(runtime_path())) {
    mkdir(runtime_path());
}

Worker::$onMasterReload = function (){
    if (function_exists('opcache_get_status')) {
        if ($status = opcache_get_status()) {
            if (isset($status['scripts']) && $scripts = $status['scripts']) {
                foreach (array_keys($scripts) as $file) {
                    opcache_invalidate($file, true);
                }
            }
        }
    }
};

Worker::$pidFile                      = $config['pid_file'];
Worker::$stdoutFile                   = $config['stdout_file'];
TcpConnection::$defaultMaxPackageSize = $config['max_package_size'] ?? 10*1024*1024;

$worker = new Worker($config['listen'], $config['context']);
if (\version_compare(\PHP_VERSION,'7.0.0', 'ge') // if php >= 7.0.0
    && \version_compare(php_uname('r'), '3.9', 'ge') // if kernel >=3.9
    && \strtolower(\php_uname('s')) !== 'darwin' // if not Mac OS
    ) { // if not unix socket
    $worker->reusePort = true;
}
$property_map = [
    'name',
    'count',
    'user',
    'group',
    'reusePort',
    'transport',
];
foreach ($property_map as $property) {
    if (isset($config[$property])) {
        $worker->$property = $config[$property];
    }
}

$worker->onWorkerStart = function ($worker) {
    set_error_handler(function ($level, $message, $file = '', $line = 0, $context = []) {
        if (error_reporting() & $level) {
            throw new ErrorException($message, 0, $level, $file, $line);
        }
    });
    register_shutdown_function(function ($start_time) {
        if (time() - $start_time <= 1) {
            sleep(1);
        }
    }, time());
    foreach (config('autoload.files', []) as $file) {
        include_once $file;
    }

    Config::reload(config_path(), ['route', 'container']);
    foreach (config('bootstrap', []) as $class_name) {
        /** @var \Webman\Bootstrap $class_name */
        $class_name::start($worker);
    }
    $app = new App($worker, Container::instance(), Log::channel('default'), app_path(), public_path());
    Route::load(config_path() . '/route.php');
    Middleware::load(config('middleware', []));
    Middleware::load(['__static__' => config('static.middleware', [])]);
    Http::requestClass(Request::class);

    $worker->onMessage = [$app, 'onMessage'];
};


Worker::runAll();
