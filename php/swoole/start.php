<?php

use Swoole\Http\Server;
use Swoole\Http\Request;
use Swoole\Http\Response;

$server = new Server('0.0.0.0', 3000);
$server->set([
    'daemonize'  => false,
    'worker_num' => swoole_cpu_num() * 2,
]);
$server->on('request', function (Request $request, Response $response) {
    $uri = $request->server['request_uri'] ?? '/';
    if (strpos($uri, '/user/') === 0 && isset($uri[6])) {
        $response->end(substr($uri, 6));
        return;
    }
    $response->end();
});

$server->start();