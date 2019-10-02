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
use Spiral\Framework\Kernel;
use Spiral\Nyholm\Bootloader as Nyholm;

class App extends Kernel
{
    /*
     * List of components and extensions to be automatically registered
     * within system container on application start.
     */
    protected const LOAD = [
        // HTTP
        Nyholm\NyholmBootloader::class,
        Bootloader\Http\HttpBootloader::class,
        Bootloader\Http\RouterBootloader::class,

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
