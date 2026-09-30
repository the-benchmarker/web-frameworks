<?php

declare(strict_types=1);

use App\Command\RoadRunnerWorker;

use function DI\add;
use function DI\autowire;
use function DI\env;

return [
    'app.root' => realpath(__DIR__ . '/../..'),

    'app.env' => env('APP_ENV'),
    'app.name' => env('APP_NAME'),
    'app.version' => env('APP_VERSION'),

    'app.commands' => add([
        autowire(RoadRunnerWorker::class),
    ]),
];
