<?php
require_once __DIR__ . '/vendor/autoload.php';


use Adapterman\Adapterman;
use Workerman\Worker;

Adapterman::init();

$http                = new Worker('http://0.0.0.0:3000');
$http->count         = (int) shell_exec('nproc');
$http->reusePort     = true;
$http->name          = 'AdapterMan-Slim';

$http->onWorkerStart = static function () {
    
    require __DIR__.'/start.php';
};

$http->onMessage = static function ($connection, $request) {

    $connection->send(run());
};

Worker::runAll();
