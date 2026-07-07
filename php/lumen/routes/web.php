<?php

/**
 * Lumen Framework Routes
 * 
 * Benchmark server routes following PHP best practices.
 */

$router->get('/', [
    'uses' => 'ApplicationController@index',
]);

$router->get('/health', [
    'uses' => 'ApplicationController@healthCheck',
]);

$router->get('/user/{id}', [
    'uses' => 'UserController@show',
]);

$router->post('/user', [
    'uses' => 'UserController@create',
]);
