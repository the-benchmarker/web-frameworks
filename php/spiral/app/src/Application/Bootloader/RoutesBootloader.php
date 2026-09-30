<?php

declare(strict_types=1);

namespace App\Application\Bootloader;

use Spiral\Bootloader\Http\RoutesBootloader as BaseRoutesBootloader;
use Spiral\Router\Bootloader\AnnotatedRoutesBootloader;
use Spiral\Router\Loader\Configurator\RoutingConfigurator;

/**
 * A bootloader that configures the application's routes and middleware.
 *
 * @link https://spiral.dev/docs/http-routing
 */
final class RoutesBootloader extends BaseRoutesBootloader
{
    protected const DEPENDENCIES = [
        AnnotatedRoutesBootloader::class,
    ];

    protected function globalMiddleware(): array
    {
        return [];
    }

    protected function middlewareGroups(): array
    {
        return [];
    }

    protected function defineRoutes(RoutingConfigurator $routes): void
    {
        $routes->default('/[<controller>[/<action>]][/<id>]')
            ->namespaced('App\\Endpoint\\Http')
            ->defaults([
                'controller' => 'home',
                'action' => 'index',
            ]);
    }
}
