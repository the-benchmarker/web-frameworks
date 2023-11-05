<?php
require __DIR__ . '/../vendor/autoload.php';

use Framework\MVC\App;
use Framework\Routing\RouteCollection;
use Framework\Routing\Router;

(new App([
    'router' => [
        'default' => [
            'callback' => function (Router $router) {
                $router->serve(null, function (RouteCollection $routes) {
                    $routes->get('/', fn () => '');
                    $routes->get('/user/{int}', fn ($args) => $args[0]);
                    $routes->post('/user', fn () => '');
                });
            },
        ],
    ],
]))->runHttp();
