<?php

use Hleb\HlebBootstrap;

define('HLEB_PUBLIC_DIR', realpath(__DIR__));
define('HLEB_GLOBAL_DIR', realpath(__DIR__ . '/../'));

require HLEB_GLOBAL_DIR . '/vendor/phphleb/framework/HlebBootstrap.php';

$config = [
    'common' => [
        'debug' => false,
        'allowed.hosts' => [],
        'log.enabled' => false,
        'max.log.level' => 'info',
        'max.cli.log.level' => 'info',
        'routes.auto-update' => true,
        'container.mock.allowed' => false,
        'app.cache.on' => false,
        'show.request.id' => false,
    ],
    'main' => [
        'session.enabled' => false,
    ],
    'system' => [
        'classes.autoload' => true,
        'classes.preload' => false,
        'events.used' => false,
        'async.clear.state' => false,
    ],
];

(new HlebBootstrap(HLEB_PUBLIC_DIR, $config))->load();
