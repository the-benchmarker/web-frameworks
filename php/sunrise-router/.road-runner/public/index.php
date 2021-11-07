<?php

declare(strict_types=1);

use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Sunrise\Http\Factory\ResponseFactory;
use Sunrise\Http\Factory\ServerRequestFactory;
use Sunrise\Http\Factory\StreamFactory;
use Sunrise\Http\Factory\UploadedFileFactory;
use Sunrise\Http\Router\Exception\MethodNotAllowedException;
use Sunrise\Http\Router\Exception\RouteNotFoundException;
use Sunrise\Http\Router\RequestHandler\CallableRequestHandler;
use Sunrise\Http\Router\Route;
use Sunrise\Http\Router\Router;

require_once __DIR__ . '/../vendor/autoload.php';

$responseFactory = new ResponseFactory();

$routes = [];

$routes[] = new Route('home', '/', ['GET'], new CallableRequestHandler(
    function (ServerRequestInterface $request) use ($responseFactory) : ResponseInterface {
        return $responseFactory->createResponse(200);
    }
));

$routes[] = new Route('user.create', '/user', ['POST'], new CallableRequestHandler(
    function (ServerRequestInterface $request) use ($responseFactory) : ResponseInterface {
        return $responseFactory->createResponse(200);
    }
));

$routes[] = new Route('user.read', '/user/{id}', ['GET'], new CallableRequestHandler(
    function (ServerRequestInterface $request) use ($responseFactory) : ResponseInterface {
        $response = $responseFactory->createResponse(200);
        $response->getBody()->write($request->getAttribute('id'));

        return $response;
    }
));

$router = new Router();
$router->addRoute(...$routes);

$worker = Spiral\RoadRunner\Worker::create();

$psrWorker = new Spiral\RoadRunner\Http\PSR7Worker(
    $worker,
    new ServerRequestFactory(),
    new StreamFactory(),
    new UploadedFileFactory()
);

while ($request = $psrWorker->waitRequest()) {
    try {
        $psrWorker->respond($router->handle($request));
    } catch (MethodNotAllowedException $e) {
        $psrWorker->respond($responseFactory->createResponse(405));
    } catch (RouteNotFoundException $e) {
        $psrWorker->respond($responseFactory->createResponse(404));
    } catch (Throwable $e) {
        $psrWorker->respond($responseFactory->createResponse(500));
    }
}
