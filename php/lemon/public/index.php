<?php

use Lemon\Kernel\Application;
use Lemon\Route;

require __DIR__.'/../vendor/autoload.php';

Application::init(__DIR__);

Route::get('/', function() {
    return '';
});

Route::get('/users/{id}', function($id) {
    return $id;
});

Route::post('/user', function() {
    return '';
});
