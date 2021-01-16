#!/usr/bin/env php
<?php

declare(strict_types=1);

use Antidot\Application\Http\Application;
use Antidot\React\PromiseResponse;
use Laminas\Diactoros\Response\HtmlResponse;
use Psr\Http\Message\ServerRequestInterface;
use Ramsey\Uuid\Uuid;
use React\EventLoop\Factory;
use React\Http\Middleware\LimitConcurrentRequestsMiddleware;
use React\Http\Middleware\RequestBodyBufferMiddleware;
use React\Http\Middleware\RequestBodyParserMiddleware;
use React\Http\Middleware\StreamingRequestMiddleware;
use React\Http\Server;
use React\Socket\Server as Socket;
use function React\Promise\resolve;

require 'vendor/autoload.php';

call_user_func(static function () {
    $loop = Factory::create();
    $container = require 'config/container.php';
    $application = $container->get(Application::class);
    (require 'router/middleware.php')($application, $container);
    (require 'router/routes.php')($application, $container);

    $server = new Server(
        $loop,
        new StreamingRequestMiddleware(),
        new LimitConcurrentRequestsMiddleware(100), // 100 concurrent buffering handlers
        new RequestBodyBufferMiddleware(4 * 1024 * 1024), // 4 MiB
        new RequestBodyParserMiddleware(),
        static function (ServerRequestInterface $request) use ($application) {
            try {
                $response = new PromiseResponse(
                    resolve($request)
                        ->then(static fn ($request) => $request->withAttribute('request_id', Uuid::uuid4()->toString()))
                        ->then(static fn ($request) => $application->handle($request))
                );
            } catch (Throwable $exception) {
                if (!empty($e = $exception->getPrevious())) {
                    $exception = $e;
                }

                $response = new HtmlResponse(
                    sprintf(
                        '%s in file %s in line %s.',
                        $exception->getMessage(),
                        $exception->getFile(),
                        $exception->getLine()
                    )
                );
            }

            return resolve($response);
        }
    );

    $server->on('error', function ($err) {
        dump($err);
    });

    $socket = new Socket('0.0.0.0:3000', $loop);
    $server->listen($socket);

    $loop->run();
});
