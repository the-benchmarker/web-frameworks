<?php

declare(strict_types=1);

use Antidot\Framework\Application;
use Psr\Container\ContainerInterface;

return static function (Application $app, ContainerInterface $container): void {
    $app->get('/', [\App\Handler\Home::class], 'home');
    $app->get('/user/{id}', [\App\Handler\UserId::class], 'user_detail');
    $app->post('/user', [\App\Handler\User::class], 'user_list');
};
