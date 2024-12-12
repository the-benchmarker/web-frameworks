<?php

use Workerman\Worker;
use Workerman\Connection\TcpConnection;
use Workerman\Protocols\Http\Response;

include __DIR__ . "/../vendor/autoload.php";

$framework = new Hleb\HlebAsyncBootstrap(__DIR__);

$server = new Worker( 'http://0.0.0.0:3000');

$server->count = shell_exec('nproc') ?: 32;

$server->onMessage = function (TcpConnection $connection, $request) use ($framework) {
    $res = $framework->load($request)->getResponse();

    $connection->send(new Response($res->getStatus(), $res->getHeaders(),  $res->getBody()));
};

Worker::runAll();
