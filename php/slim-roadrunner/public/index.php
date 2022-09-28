<?php

use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use Spiral\Goridge\StreamRelay;
use Slim\Factory\AppFactory;
use Slim\Psr7\Factory\ServerRequestFactory;
use Slim\Psr7\Factory\StreamFactory;
use Slim\Psr7\Factory\UploadedFileFactory;
use Spiral\RoadRunner\Http\PSR7Worker;
use Spiral\RoadRunner\Worker;

ini_set('display_errors', 'stderr');

require __DIR__ . '/../vendor/autoload.php';

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

$worker = new PSR7Worker(
    Worker::create(),
    new ServerRequestFactory(),
    new StreamFactory(),
    new UploadedFileFactory()
);

while ($req = $worker->waitRequest()) {
    try {
        $worker->respond($app->handle($req));
    } catch (\Throwable $e) {
        $worker->getWorker()->error((string)$e);
    }
}