<?php

use Lemon\Kernel\Application;
use Lemon\Protection\Middlwares\Csrf;
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
})->exclude(Csrf::class); // Since Lemon by default checks csrf, we have to manualy disable it.
