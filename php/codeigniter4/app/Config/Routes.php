<?php

use CodeIgniter\Router\RouteCollection;

/**
 * @var RouteCollection $routes
 */
$routes->get('/', 'Home::index');
$routes->get('user/(:num)', 'User::view/$1');
$routes->post('user', 'User::list');
