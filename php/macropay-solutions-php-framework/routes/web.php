<?php

use App\Http\Controllers\BenchmarkController;

/** @var \App\Router $router */

$router->get('/', [
    'as' => 'index',
    'uses' => [BenchmarkController::class, 'index'],
]);

$router->get('/user/{id}', [
    'as' => 'user',
    'uses' => [BenchmarkController::class, 'user'],
]);

$router->post('/user', [
    'as' => 'user.create',
    'uses' => [BenchmarkController::class, 'createUser'],
]);

$router->get('/serialization', [
    'as' => 'serialization',
    'uses' => [BenchmarkController::class, 'serialization'],
]);

$router->post('/deserialization', [
    'as' => 'deserialization',
    'uses' => [BenchmarkController::class, 'deserialization'],
]);

$router->post('/upload', [
    'as' => 'upload',
    'uses' => [BenchmarkController::class, 'upload'],
]);
