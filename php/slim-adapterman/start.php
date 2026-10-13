<?php

use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use Slim\Factory\AppFactory;

require __DIR__ . '/vendor/autoload.php';

global $app;

$app = AppFactory::create();

$app->get('/', function (Request $request, Response $response): Response {
    $response->getBody()->write("");
    return $response;
});

$app->get('/user/{id}', function (Request $request, Response $response, array $args): Response {
    $response->getBody()->write($args["id"]);
    return $response;
});

$app->post('/user', function (Request $request, Response $response): Response {
    $response->getBody()->write("");
    return $response;
});

//$app->run();
function run(): string
{
    global $app;
    ob_start();

    $app->run();

    return ob_get_clean();
}
