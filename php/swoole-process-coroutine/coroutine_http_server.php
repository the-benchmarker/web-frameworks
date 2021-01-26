<?php

use Swoole\Process\Pool;
use Swoole\Coroutine\Http\Server;

$worker_num = swoole_cpu_num() * 2;

$pool = new Pool($worker_num);
$pool->set(['enable_coroutine' => true]);
$pool->on('workerStart', function ($pool, $id) {
    $server = new Server('0.0.0.0', 3000, false, true);
    $server->handle('/', function ($request, $response) {
        $response->end('');
    });
    $server->handle('/user', function ($request, $response) {
        $uri = $request->server['request_uri'] ?? '/';
        if (strpos($uri, '/user/') === 0 && isset($uri[6])) {
            $response->end(substr($uri, 6));
            return;
        }
        $response->end();
    });
    $server->start();
});
$pool->start();
