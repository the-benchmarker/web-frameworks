<?php
/**
 * Spiral Framework.
 *
 * @license   MIT
 * @author    Anton Titov (Wolfy-J)
 */
declare(strict_types=1);

namespace App;

use App\Bootloader\RoutesBootloader;
use Spiral\Bootloader;
use Spiral\DotEnv\Bootloader as DotEnv;
use Spiral\Framework\Kernel;
use Spiral\Nyholm\Bootloader as Nyholm;

class App extends Kernel
{
    /*
     * List of components and extensions to be automatically registered
     * within system container on application start.
     */
    protected const LOAD = [
        // Environment configuration
        DotEnv\DotenvBootloader::class,

        // HTTP Bootloader
        Bootloader\Http\HttpBootloader::class,

        // HTTP extensions
        Nyholm\NyholmBootloader::class,
        Bootloader\Http\RouterBootloader::class,
        Bootloader\Http\ErrorHandlerBootloader::class,

        // Framework commands
        Bootloader\CommandBootloader::class
    ];

    /*
     * Application specific services and extensions.
     */
    protected const APP = [
        RoutesBootloader::class,
    ];
}
