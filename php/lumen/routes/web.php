<?php

$router->get('/', [
    'uses' => 'ApplicationController@index',
]);

$router->get('/user/{id:[0-9]+}', [
    'uses' => 'UserController@show',
]);

$router->post('/user', [
    'uses' => 'UserController@create',
]);
