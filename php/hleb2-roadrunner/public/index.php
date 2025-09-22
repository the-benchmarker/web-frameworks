<?php

use Spiral\RoadRunner;
use Nyholm\Psr7;

ini_set('display_errors', 'stderr');

include __DIR__ . "/../vendor/autoload.php";

$worker = RoadRunner\Worker::create();
$psrFactory = new Psr7\Factory\Psr17Factory();

$psr7 = new RoadRunner\Http\PSR7Worker($worker, $psrFactory, $psrFactory, $psrFactory);

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

$app = new Hleb\HlebAsyncBootstrap(__DIR__, $config);

while ($request = $psr7->waitRequest()) {
    try {
        $response = $app->load($request)->getResponse();
        $psr7->respond(new Psr7\Response(...$response->getArgs()));
    } catch (\Throwable $e) {
        $app->errorLog($e);
    }
}
