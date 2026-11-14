<?php

require_once __DIR__ . '/vendor/autoload.php';

use Amp\Cluster\Cluster;
use Amp\Http\Server\DefaultErrorHandler;
use Amp\Http\Server\Driver\SocketClientFactory;
use Amp\Http\Server\Request;
use Amp\Http\Server\RequestHandler\ClosureRequestHandler;
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

$router->addRoute('GET', '/', new ClosureRequestHandler(
    static fn (Request $request): Response => new Response(),
));

$router->addRoute('GET', '/user/{id}', new ClosureRequestHandler(
    static fn (Request $request): Response => new Response(body: $request->getAttribute(Router::class)['id']),
));

$router->addRoute('POST', '/user', new ClosureRequestHandler(
    static fn (Request $request): Response => new Response(),
));

$server->start($router, $errorHandler);

Cluster::awaitTermination();

$server->stop();
