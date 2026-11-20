<?php

declare(strict_types=1);

use Antidot\Framework\Application;
use Antidot\Runtime\AntidotRuntime;
use Psr\Container\ContainerInterface;

ini_set('memory_limit', '2048M');

$_SERVER['APP_RUNTIME'] = AntidotRuntime::class;

chdir(dirname(__DIR__));
$rootDir = dirname(__DIR__);

require_once 'vendor/autoload_runtime.php';

return static function () use ($rootDir): ContainerInterface {
    $container = require $rootDir . '/config/container.php';
    $application = $container->get(Application::class);

    (require $rootDir . '/router/middleware.php')($application);
    (require $rootDir . '/router/routes.php')($application, $container);

    return $container;
};
