<?php

declare(strict_types=1);

use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Sunrise\Http\Message\ResponseFactory;
use Sunrise\Http\Router\Exception\MethodNotAllowedException;
use Sunrise\Http\Router\Exception\RouteNotFoundException;
use Sunrise\Http\Router\RequestHandler\CallableRequestHandler;
use Sunrise\Http\Router\RouteCollector;
use Sunrise\Http\Router\Router;
use Sunrise\Http\ServerRequest\ServerRequestFactory;

use function Sunrise\Http\Router\emit;

require_once __DIR__ . '/../vendor/autoload.php';

$router = new Router();
$routeCollector = new RouteCollector();

$routeCollector->get('home', '/', new CallableRequestHandler(function (ServerRequestInterface $request) : ResponseInterface {
    return (new ResponseFactory)->createResponse(200);
}));

$routeCollector->get('userRead', '/user/{id}', new CallableRequestHandler(function (ServerRequestInterface $request) : ResponseInterface {
    $response = (new ResponseFactory)->createResponse(200);
    $response->getBody()->write($request->getAttribute('id'));

    return $response;
}));

$routeCollector->post('userCreate', '/user', new CallableRequestHandler(function (ServerRequestInterface $request) : ResponseInterface {
    return (new ResponseFactory)->createResponse(201);
}));

$router->addRoute(...$routeCollector->getCollection()->all());

try {
    emit($router->handle(ServerRequestFactory::fromGlobals()));
} catch (MethodNotAllowedException $e) {
    emit((new ResponseFactory)->createResponse(405));
} catch (RouteNotFoundException $e) {
    emit((new ResponseFactory)->createResponse(404));
}
