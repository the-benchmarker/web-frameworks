<?php

/** @var Fomo\Router\Router $router */

use App\Controllers\BenchmarkController;

$router->get('/' , [BenchmarkController::class , 'index']);
$router->get('user/{id}' , [BenchmarkController::class , 'getUser']);
$router->post('user' , [BenchmarkController::class , 'postUser']);