<?php

use Cake\Routing\RouteBuilder;

return function (RouteBuilder $routes): void {
    $routes->scope('/', function (RouteBuilder $builder): void {
        $builder->get('/', ['controller' => 'Application', 'action' => 'index']);
        $builder->get('/user/{id}', ['controller' => 'User', 'action' => 'view'])->setPass(['id']);
        $builder->post('/user', ['controller' => 'User', 'action' => 'add']);
    });
};
