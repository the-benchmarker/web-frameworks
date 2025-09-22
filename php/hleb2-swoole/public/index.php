<?php

use Swoole\Http\Request;
use Swoole\Http\Response;
use Swoole\Http\Server;

include __DIR__ . "/../vendor/autoload.php";

$http = new Server('0.0.0.0', 3000, SWOOLE_BASE, SWOOLE_SOCK_TCP);
$http->set([
    'worker_num' => swoole_cpu_num() * 2,
    'enable_coroutine' => false,
    'log_file' => '/dev/null',
    'log_level' => SWOOLE_LOG_ERROR,
]);

$config = [
    'common' => [
        'debug' => false,
        'allowed.hosts' => ['0.0.0.0', 'localhost'],
        'log.enabled' => false,
        'max.log.level' => 'info',
        'max.cli.log.level' => 'info',
        'routes.auto-update' => true,
        'container.mock.allowed' => false,
        'app.cache.on' => false,
        'show.request.id' => false,
    ],
    'main' => [
        'session.enabled' => false,
    ],
    'system' => [
        'classes.autoload' => true,
        'classes.preload' => false,
        'events.used' => false,
        'async.clear.state' => false,
    ],
];

$app = new Hleb\HlebAsyncBootstrap(__DIR__, $config);

$http->on('request', function ($request, Response $response) use ($app) {
    $result = $app->load($request)->getResponse();
    foreach ($result->getHeaders() as $name => $header) {
        $response->header($name, $header);
    }
    $response->status($result->getStatus(), (string)$result->getReason());
    $response->end($result->getBody());
});

$http->start();
