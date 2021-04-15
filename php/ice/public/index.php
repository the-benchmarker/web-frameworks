<?php

$di = new \Ice\Di();

$di->loader
    ->addNamespace('App', __DIR__ . '/../App')
    ->register();

$di->dispatcher
    ->setNamespace('App');

$di->router
   ->setRoutes([
    ['*', '[/{action}[/{id}]]', ['action' => '\w+']],
    ['GET', '/']
   ]);

$app = new \Ice\Mvc\App($di);

echo $app->handle();
