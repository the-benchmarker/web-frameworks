<?php

use Slim\App;
use Slim\Http;

require_once __DIR__ . '/../vendor/autoload.php';

$app = new App(['settings' => ['routerCacheFile' => __DIR__ . '/../var/routes.cache.php']]);

$app->get('/', function(Http\Request $request, Http\Response $response): Http\Response {
    return $response->write('');
});

$app->get('/user/{id}', function(Http\Request $request, Http\Response $response, array $args): Http\Response {
    return $response->write($args['id']);
});

$app->post('/user', function(Http\Request $request, Http\Response $response): Http\Response {
    return $response->write('');
});

$app->run();
