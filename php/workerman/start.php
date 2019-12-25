<?php
require_once __DIR__ . '/vendor/autoload.php';
ini_set('pcre.jit', 0);

use Workerman\Worker;

$worker = new Worker('http://0.0.0.0:3000');
$worker->count = shell_exec('nproc') ? shell_exec('nproc') : 32;
$worker->onMessage = function ($connection, $data) {
    $request_uri = $data['server']['REQUEST_URI'];
    switch ($request_uri) {
        case '/':
        case '/user':
            $connection->send('');
            return;
        default:
            if (preg_match('/^\/user\/(.+)$/', $request_uri, $match)) {
                $connection->send($match[1]);
                return;
            }
            $connection->send('');
    }
};
Worker::runAll();
