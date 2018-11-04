<?php

$router->get('/', [
    'uses' => 'ApplicationController@index',
]);

$router->get('/user/{id}', [
    'uses' => 'UserController@show',
]);

$router->post('/user', [
    'uses' => 'UserController@create',
]);
