<?php

declare(strict_types=1);

use Antidot\Framework\Application;
use Antidot\Framework\Middleware\ErrorMiddleware;
use Antidot\Framework\Middleware\RouteDispatcherMiddleware;
use Antidot\Framework\Middleware\RouteNotFoundMiddleware;

return static function (Application $app): void {
    $app->pipe(ErrorMiddleware::class);
    $app->pipe(RouteDispatcherMiddleware::class);
    $app->pipe(RouteNotFoundMiddleware::class);
};
