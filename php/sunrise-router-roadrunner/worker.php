<?php

declare(strict_types=1);

use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Sunrise\Http\Router\Exception\MethodNotAllowedException;
use Sunrise\Http\Router\Exception\RouteNotFoundException;
use Sunrise\Http\Router\RequestHandler\CallableRequestHandler;
use Sunrise\Http\Router\RouteCollector;
use Sunrise\Http\Router\Router;
use Sunrise\Http\Factory\ResponseFactory;
use Sunrise\Http\Factory\ServerRequestFactory;
use Sunrise\Http\Factory\StreamFactory;
use Sunrise\Http\Factory\UploadedFileFactory;

use Spiral\Goridge\StreamRelay as RoadRunnerRelay;
use Spiral\RoadRunner\PSR7Client as RoadRunnerClient;
use Spiral\RoadRunner\Worker as RoadRunnerWorker;

require_once __DIR__ . '/vendor/autoload.php';

$router = new Router();
$routeCollector = new RouteCollector();
$responsePrototype = (new ResponseFactory)
    ->createResponse(200);

$routeCollector->get('home', '/', new CallableRequestHandler(
    function (ServerRequestInterface $request) use ($responsePrototype) : ResponseInterface {
        return $responsePrototype;
    }
));

$routeCollector->get('userRead', '/user/{id}', new CallableRequestHandler(
    function (ServerRequestInterface $request) use ($responsePrototype) : ResponseInterface {
        $responsePrototype->getBody()->write(
            $request->getAttribute('id')
        );

        return $responsePrototype;
    }
));

$routeCollector->post('userCreate', '/user', new CallableRequestHandler(
    function (ServerRequestInterface $request) use ($responsePrototype) : ResponseInterface {
        return $responsePrototype->withStatus(201);
    }
));

$router->addRoute(
    ...$routeCollector->getCollection()->all()
);

$relay = new RoadRunnerRelay(STDIN, STDOUT);

$client = new RoadRunnerClient(
    new RoadRunnerWorker($relay),
    new ServerRequestFactory(),
    new StreamFactory(),
    new UploadedFileFactory()
);

while ($request = $client->acceptRequest()) {
    try {
        $client->respond(
            $router->handle($request)
        );
    } catch (MethodNotAllowedException $e) {
        $client->respond(
            $responsePrototype->withStatus(405)
        );
    } catch (RouteNotFoundException $e) {
        $client->respond(
            $responsePrototype->withStatus(404)
        );
    } catch (Throwable $e) {
        $client->respond(
            $responsePrototype->withStatus(500)
        );
    }
}
