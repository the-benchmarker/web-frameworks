<?php
require __DIR__ . '/../vendor/autoload.php';

$vega = App\Vega::new();
$http = new Swoole\Http\Server('0.0.0.0', 3000, SWOOLE_BASE, SWOOLE_SOCK_TCP);
$http->on('Request', $vega->handler());
$http->set([
    'worker_num' => swoole_cpu_num() * 2,
    'enable_coroutine' => false,
    'log_file' => '/dev/null',
    'log_level' => SWOOLE_LOG_ERROR,
]);
$http->start();
