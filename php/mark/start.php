<?php

use Mark\App;

require 'vendor/autoload.php';

$api = new App('http://0.0.0.0:3000');

$api->count = 7;

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
