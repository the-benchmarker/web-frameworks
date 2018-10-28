<?php

use Slim\App;
use Slim\Http\Request;
use Slim\Http\Response;

require_once __DIR__.'/../vendor/autoload.php';

$app = new App(['settings' => ['routerCacheFile' => __DIR__.'/../var/routes.cache.php']]);

$app->get('/', function (Request $request, Response $response): Response {
    return $response->write('');
});

$app->get('/user/{id}', function (Request $request, Response $response, array $args): Response {
    return $response->write($args['id']);
});

$app->post('/user', function (Request $request, Response $response): Response {
    return $response->write('');
});

$app->run();
