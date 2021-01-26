<?php

use Swoole\Http\Server;
use Swoole\Http\Request;
use Swoole\Http\Response;

$server = new Server('0.0.0.0', 3000);
$server->set(
    [
        'worker_num' => swoole_cpu_num() * 2,
        'enable_coroutine' => false,
        'log_file' => '/dev/null',
        'log_level' => SWOOLE_LOG_ERROR,
    ]
);
$server->on('request', function (Request $request, Response $response) {
        $uri = $request->server['request_uri'] ?? '/';
        if (strpos($uri, '/user/') === 0 && isset($uri[6])) {
            $response->end(substr($uri, 6));
            return;
        }
        $response->end();
});

$server->start();
