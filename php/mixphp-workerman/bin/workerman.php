<?php
require __DIR__ . '/../vendor/autoload.php';

$vega = App\Vega::new();
$http = new Workerman\Worker("http://0.0.0.0:3000");
$http->onMessage = $vega->handler();
$http->count = (int)(shell_exec('nproc') ?: 8) * 2;
Workerman\Worker::runAll();
