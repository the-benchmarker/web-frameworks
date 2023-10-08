<?php
use Framework\Routing\RouteCollection;

App::router()->serve(null, static function (RouteCollection $routes) : void {
    $routes->namespace('App\Controllers', [
        $routes->get('/', 'Home::index', 'home'),
        $routes->get('/user/{int}', 'User::show/0', 'user.show'),
        $routes->post('/user', 'User::create', 'user.create'),
    ]);
});
