<?php

declare(strict_types=1);

use DI\Container;
use DI\ContainerBuilder;

return (static function (): Container {
    $appEnv = $_ENV['APP_ENV'] ?? 'dev';

    $containerBuilder = new ContainerBuilder();

    // https://github.com/sunrise-php/http-message
    $containerBuilder->addDefinitions(
        __DIR__ . '/../vendor/sunrise/http-message/resources/definitions/psr17.php',
    );

    // https://github.com/sunrise-php/http-router
    $containerBuilder->addDefinitions(
        __DIR__ . '/../vendor/sunrise/http-router/resources/definitions/router.php',
        __DIR__ . '/../vendor/sunrise/http-router/resources/definitions/loaders/descriptor_loader.php',
    );

    $containerBuilder->addDefinitions(
        ...glob(__DIR__ . '/definitions/*.php'),
        ...glob(__DIR__ . '/definitions/' . $appEnv . '/*.php'),
    );

    if ($appEnv === 'prod') {
        $containerBuilder->enableCompilation(__DIR__ . '/../var/cache/container');
    }

    return $containerBuilder->build();
})();
