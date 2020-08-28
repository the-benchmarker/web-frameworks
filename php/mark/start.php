<?php
use Mark\SimpleApp;

require 'vendor/autoload.php';

$api = new SimpleApp('tcp://0.0.0.0:3000');

$api->count = shell_exec('nproc') ? shell_exec('nproc') - 4 : 12;

$api->get('/', function ($requst) {
    return '';
});

$api->post('/user', function ($requst) {
    return '';
});

$api->get('/user/{id}', function ($requst, $id) {
    return $id;
});

$api->start();
