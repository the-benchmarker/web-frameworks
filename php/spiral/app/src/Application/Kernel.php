<?php

declare(strict_types=1);

namespace App\Application;

use Spiral\Boot\Bootloader\CoreBootloader;
use Spiral\Bootloader as Framework;
use Spiral\DotEnv\Bootloader\DotenvBootloader;
use Spiral\Monolog\Bootloader\MonologBootloader;
use Spiral\Nyholm\Bootloader\NyholmBootloader;
use Spiral\Prototype\Bootloader\PrototypeBootloader;
use Spiral\RoadRunnerBridge\Bootloader as RoadRunnerBridge;
use Spiral\Tokenizer\Bootloader\TokenizerListenerBootloader;
use Spiral\Validation\Bootloader\ValidationBootloader;
use Spiral\Validator\Bootloader\ValidatorBootloader;
use Spiral\YiiErrorHandler\Bootloader\YiiErrorHandlerBootloader;

class Kernel extends \Spiral\Framework\Kernel
{
    protected const SYSTEM = [
        CoreBootloader::class,
        TokenizerListenerBootloader::class,
        DotenvBootloader::class,
    ];

    protected const LOAD = [
        // RoadRunner
        RoadRunnerBridge\HttpBootloader::class,

        // Core Services
        Framework\SnapshotsBootloader::class,

        // HTTP extensions
        Framework\Http\RouterBootloader::class,
        NyholmBootloader::class,

        // Console commands
        Framework\CommandBootloader::class,
        RoadRunnerBridge\CommandBootloader::class,

        // Configure route groups, middleware for route groups
        Bootloader\RoutesBootloader::class,
    ];

    protected const APP = [];
}
