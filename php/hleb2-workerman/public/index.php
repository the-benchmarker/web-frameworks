<?php

use Workerman\Worker;
use Workerman\Connection\TcpConnection;
use Workerman\Protocols\Http\Response;

include __DIR__ . "/../vendor/autoload.php";

$config = [
    'common' => [
        'debug' => false,
        'allowed.hosts' => [],
        'log.enabled' => false,
        'max.log.level' => 'info',
        'max.cli.log.level' => 'info',
        'error.reporting' => E_ALL,
        'timezone' => 'UTC',
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

$framework = new Hleb\HlebAsyncBootstrap(__DIR__, $config);

$server = new Worker( 'http://0.0.0.0:3000');

$server->count = shell_exec('nproc') ?: 32;

$server->onMessage = function (TcpConnection $connection, $request) use ($framework) {
    $res = $framework->load($request)->getResponse();

    $connection->send(new Response($res->getStatus(), $res->getHeaders(),  $res->getBody()));
};

Worker::runAll();
