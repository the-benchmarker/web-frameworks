<?php

require_once __DIR__ . '/vendor/autoload.php';

use Workerman\Worker;

$worker = new Worker('http://0.0.0.0:3000');
$worker->count = shell_exec('nproc') ?: 32;
$worker->onMessage = static function ($connection, $request) {
    $path = $request->path();
    match ($path) {
        '/', '/user' => $connection->send(''),           
        default => $connection->send( \str_starts_with($path, '/user/') ? \substr($path, 6) : ''),
    };
};
Worker::runAll();
