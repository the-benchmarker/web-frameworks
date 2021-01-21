#!/usr/bin/env php
<?php

declare(strict_types=1);

use Antidot\Application\Http\Application;
use Psr\Log\LoggerInterface;
use React\EventLoop\LoopInterface;
use React\Http\Server;
use React\Socket\Server as Socket;

require 'vendor/autoload.php';

call_user_func(static function () {
    $container = require 'config/container.php';
    $application = $container->get(Application::class);
    (require 'router/middleware.php')($application, $container);
    (require 'router/routes.php')($application, $container);

    $loop = $container->get(LoopInterface::class);

    $server = $container->get(Server::class);
    $server->on('error', static function ($err) use ($container) {
        $logger = $container->get(LoggerInterface::class);
        $logger->critical($err);
    });

    $socket = $container->get(Socket::class);
    $server->listen($socket);

    $loop->run();
});
