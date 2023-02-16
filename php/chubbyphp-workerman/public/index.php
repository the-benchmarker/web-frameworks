<?php

declare(strict_types=1);

namespace App;

use Chubbyphp\Framework\Application;
use Chubbyphp\Framework\Middleware\ExceptionMiddleware;
use Chubbyphp\Framework\Middleware\RouteMatcherMiddleware;
use Chubbyphp\Framework\Router\FastRoute\RouteMatcher;
use Chubbyphp\Framework\Router\Route;
use Chubbyphp\Framework\Router\RoutesByName;
use Chubbyphp\WorkermanRequestHandler\OnMessage;
use Chubbyphp\WorkermanRequestHandler\PsrRequestFactory;
use Chubbyphp\WorkermanRequestHandler\WorkermanResponseEmitter;
use Psr\Http\Message\ResponseFactoryInterface;
use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Psr\Http\Server\RequestHandlerInterface;
use Slim\Psr7\Factory\ResponseFactory;
use Slim\Psr7\Factory\ServerRequestFactory;
use Slim\Psr7\Factory\StreamFactory;
use Slim\Psr7\Factory\UploadedFileFactory;
use Workerman\Worker;

$loader = require __DIR__ . '/../vendor/autoload.php';

$responseFactory = new ResponseFactory();

$app = new Application([
    new ExceptionMiddleware($responseFactory, true),
    new RouteMatcherMiddleware(new RouteMatcher(new RoutesByName([
        Route::get('/', 'home', new class($responseFactory) implements RequestHandlerInterface {
            public function __construct(private ResponseFactoryInterface $responseFactory) {}
            public function handle(ServerRequestInterface $request): ResponseInterface {
                return $this->responseFactory->createResponse();
            }
        }),
        Route::get('/user/{id}', 'user_view', new class($responseFactory) implements RequestHandlerInterface {
            public function __construct(private ResponseFactoryInterface $responseFactory) {}
            public function handle(ServerRequestInterface $request): ResponseInterface {
                $response = $this->responseFactory->createResponse();
                $response->getBody()->write($request->getAttribute('id'));

                return $response;
            }
        }),
        Route::post('/user', 'user_list', new class($responseFactory) implements RequestHandlerInterface {
            public function __construct(private ResponseFactoryInterface $responseFactory) {}
            public function handle(ServerRequestInterface $request): ResponseInterface {
                return $this->responseFactory->createResponse();
            }
        }),
    ]), sys_get_temp_dir() . '/chubbyphp-workerman.php')),
]);

$server = new Worker('http://0.0.0.0:3000');

$server->count = shell_exec('nproc') ? shell_exec('nproc') : 32;

$server->onMessage = new OnMessage(
    new PsrRequestFactory(
        new ServerRequestFactory(),
        new StreamFactory(),
        new UploadedFileFactory()
    ),
    new WorkermanResponseEmitter(),
    $app
);

Worker::runAll();
