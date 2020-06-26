<?php

declare(strict_types=1);

namespace App;

use Chubbyphp\Framework\Application;
use Chubbyphp\Framework\ErrorHandler;
use Chubbyphp\Framework\Middleware\ExceptionMiddleware;
use Chubbyphp\Framework\Middleware\RouterMiddleware;
use Chubbyphp\Framework\RequestHandler\CallbackRequestHandler;
use Chubbyphp\Framework\Router\FastRoute\Router;
use Chubbyphp\Framework\Router\Route;
use Psr\Http\Message\ServerRequestInterface;
use Slim\Psr7\Factory\ResponseFactory;
use Slim\Psr7\Factory\ServerRequestFactory;

$loader = require __DIR__.'/../vendor/autoload.php';

set_error_handler([new ErrorHandler(), 'errorToException']);

$responseFactory = new ResponseFactory();

$app = new Application([
    new ExceptionMiddleware($responseFactory, true),
    new RouterMiddleware(new Router([
        Route::get('/', 'home', new CallbackRequestHandler(
            function () use ($responseFactory) {
                $response = $responseFactory->createResponse();
                $response->getBody()->write('');

                return $response;
            }
        )),
        Route::get('/user/{id}', 'user_view', new CallbackRequestHandler(
            function (ServerRequestInterface $request) use ($responseFactory) {
                $response = $responseFactory->createResponse();
                $response->getBody()->write($request->getAttribute('id'));

                return $response;
            }
        )),
        Route::post('/user', 'user_list', new CallbackRequestHandler(
            function () use ($responseFactory) {
                $response = $responseFactory->createResponse();
                $response->getBody()->write('');

                return $response;
            }
        ))
    ]), $responseFactory),
]);

$app->emit($app->handle((new ServerRequestFactory())->createFromGlobals()));
