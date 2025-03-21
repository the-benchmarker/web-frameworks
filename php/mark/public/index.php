<?php

use Mark\App;

require __DIR__.'/../vendor/autoload.php';

$api = new App('http://0.0.0.0:3000');

$api->count = 7;

$api->get('/', fn() => '');

$api->post('/user', fn() => '');

$api->get('/user/{id}', fn($request, $id) => $id);

$api->start();
