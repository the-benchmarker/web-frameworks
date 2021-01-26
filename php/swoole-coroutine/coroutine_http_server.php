<?php

use Swoole\Coroutine;
use Swoole\Coroutine\Http\Server;

Coroutine\run(function () {
    $server = new Server('127.0.0.1', 3000);
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
