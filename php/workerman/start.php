<?php

require_once __DIR__ . '/vendor/autoload.php';

use Workerman\Worker;

$worker = new Worker('http://0.0.0.0:3000');
$worker->count = shell_exec('nproc') ? shell_exec('nproc') : 32;
$worker->onMessage = function ($connection, $request) {
    $path = $request->path();
    switch ($path) {
        case '/':
        case '/user':
            $connection->send('');
            return;
        default:
            if (0 === \strpos($path, '/user/') && isset($path[6])) {
                $connection->send(\substr($path, 6));
                return;
            }
            $connection->send('');
    }
};
Worker::runAll();
