<?php

require(__DIR__.'/../vendor/autoload.php');

use Unic\App;
use Swoole\Http\Server;

$app = new App();
$server = new Server('localhost', 3000);

$server->set([
    'worker_num' => swoole_cpu_num() * 2,
    'enable_coroutine' => false,
    'log_file' => '/dev/null',
    'log_level' => SWOOLE_LOG_ERROR,
]);

$app->get('/', function ($req, $res) {
    $res->send('');
});

$app->get('/user/{id}', function ($req, $res) {
    $res->send($req->params->id);
});

$app->post('/user', function ($req, $res) {
    $res->send('');
});

$app->useOpenSwooleServer($server);
$app->start();
