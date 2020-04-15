<?php

declare(strict_types=1);
/**
 * This file is part of Hyperf.
 *
 * @link     https://www.hyperf.io
 * @document https://doc.hyperf.io
 * @contact  group@hyperf.io
 * @license  https://github.com/hyperf/hyperf/blob/master/LICENSE
 */
use Hyperf\HttpServer\Router\Router;

Router::addRoute(['GET'], '/', 'App\Controller\IndexController@index');
Router::addRoute(['GET'], '/user/{id:\d+}', 'App\Controller\IndexController@get');
Router::addRoute(['POST'], '/user', 'App\Controller\IndexController@create');
