<?php

use Spiral\RoadRunner;
use Nyholm\Psr7;

ini_set('display_errors', 'stderr');

include __DIR__ . "/../vendor/autoload.php";

$worker = RoadRunner\Worker::create();
$psrFactory = new Psr7\Factory\Psr17Factory();

$psr7 = new RoadRunner\Http\PSR7Worker($worker, $psrFactory, $psrFactory, $psrFactory);

$app = new Hleb\HlebAsyncBootstrap(__DIR__);

while ($request = $psr7->waitRequest()) {
    try {
        $response = $app->load($request)->getResponse();
        $psr7->respond(new Psr7\Response(...$response->getArgs()));
    } catch (\Throwable $e) {
        $app->errorLog($e);
    }
}
