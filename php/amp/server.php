<?php

require_once __DIR__ . '/vendor/autoload.php';

use Amp\Cluster\Cluster;
use Amp\Http\Server\DefaultErrorHandler;
use Amp\Http\Server\Driver\SocketClientFactory;
use Amp\Http\Server\Request;
use Amp\Http\Server\RequestHandler;
use Amp\Http\Server\Response;
use Amp\Http\Server\Router;
use Amp\Http\Server\SocketHttpServer;
use Psr\Log\NullLogger;

$logger = new NullLogger();

$server = new SocketHttpServer(
    $logger,
    Cluster::getServerSocketFactory(),
    new SocketClientFactory($logger),
);

$server->expose('0.0.0.0:3000');

$errorHandler = new DefaultErrorHandler();

$router = new Router($server, $logger, $errorHandler);

$router->addRoute('GET', '/', new class implements RequestHandler {
    public function handleRequest(Request $request): Response
    {
        return new Response();
    }
});

$router->addRoute('GET', '/user/{id}', new class implements RequestHandler {
    public function handleRequest(Request $request): Response
    {
        return new Response(body: $request->getAttribute(Router::class)['id']);
    }
});

$router->addRoute('POST', '/user', new class implements RequestHandler {
    public function handleRequest(Request $request): Response
    {
        return new Response();
    }
});

$server->start($router, $errorHandler);

Cluster::awaitTermination();

$server->stop();
