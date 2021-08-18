<?php
require __DIR__ . '/../vendor/autoload.php';

$vega = App\Vega::new();
$http = new Swoole\Http\Server('0.0.0.0', 3000);
$http->on('Request', $vega->handler());
$http->set([
    'enable_coroutine' => false,
    'worker_num' => swoole_cpu_num() * 2,
    'log_file' => '/dev/null',
    'log_level' => 5,
]);
$http->start();
