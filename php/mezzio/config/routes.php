<?php

declare(strict_types=1);

use App\Handler\HomePageHandler;
use App\Handler\PingHandler;
use Mezzio\Application;
use Mezzio\MiddlewareFactory;
use Psr\Container\ContainerInterface;

return static function (Application $app, MiddlewareFactory $factory, ContainerInterface $container): void {
	$app->route('/', \App\Handler\Home::class, ['GET']);
    $app->route('/user/{id}', \App\Handler\UserId::class, ['GET']);
    $app->route('/user', \App\Handler\User::class, ['POST']);
};
