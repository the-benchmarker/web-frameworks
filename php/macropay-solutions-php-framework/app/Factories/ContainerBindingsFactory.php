<?php

namespace App\Factories;

use App\Console\Kernel;
use App\Exceptions\Handler;

class ContainerBindingsFactory
{
    public static function createExceptionHandler(): Handler
    {
        return new Handler();
    }

    public static function createConsoleKernel($app): Kernel
    {
        return new Kernel($app);
    }
}
