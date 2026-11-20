<?php

require_once __DIR__ . '/vendor/autoload.php';

! defined('WORKERS_COUNT') && define('WORKERS_COUNT', shell_exec('nproc'));

use Comet\Comet;

$app = new Comet([
    'port'    => 3000,
    'workers' => intval(WORKERS_COUNT),
]);


$app->get('/', function ($request, $response) {
    return $response;
});

$app->post('/user', function ($request, $response) {
    return $response;
});

$app->get('/user/{id}', function ($request, $response, $args) {
    return $response
        ->with($args['id']);
});

$app->run();
