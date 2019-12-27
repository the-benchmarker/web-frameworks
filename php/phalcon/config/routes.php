<?php
declare(strict_types=1);

use Phalcon\Mvc\Router;

/**
 * @var $router Router
 */

$router->add('/', [
    'controller' => 'index',
    'action'     => 'index',
]);

$router->add('/user', [
    'controller' => 'user',
    'action'     => 'create'
], ['POST']);

$router->add('/user/([0-9])', [
    'controller' => 'user',
    'action'     => 'show',
    'id' => 1
]);
