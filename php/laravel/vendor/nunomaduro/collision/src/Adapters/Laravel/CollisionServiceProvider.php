<?php

/**
 * This file is part of Collision.
 *
 * (c) Nuno Maduro <enunomaduro@gmail.com>
 *
 *  For the full copyright and license information, please view the LICENSE
 *  file that was distributed with this source code.
 */

namespace NunoMaduro\Collision\Adapters\Laravel;

use NunoMaduro\Collision\Provider;
use Illuminate\Support\ServiceProvider;
use NunoMaduro\Collision\Adapters\Phpunit\Listener;
use NunoMaduro\Collision\Contracts\Provider as ProviderContract;
use Illuminate\Contracts\Debug\ExceptionHandler as ExceptionHandlerContract;
use NunoMaduro\Collision\Contracts\Adapters\Phpunit\Listener as ListenerContract;

/**
 * This is an Collision Laravel Adapter Service Provider implementation.
 *
 * Registers the Error Handler on Laravel.
 *
 * @author Nuno Maduro <enunomaduro@gmail.com>
 */
class CollisionServiceProvider extends ServiceProvider
{
    /**
     * {@inheritdoc}
     */
    protected $defer = true;

    /**
     * {@inheritdoc}
     */
    public function register()
    {
        if ($this->app->runningInConsole() && ! $this->app->runningUnitTests()) {
            $this->app->singleton(ListenerContract::class, Listener::class);
            $this->app->bind(ProviderContract::class, Provider::class);

            $appExceptionHandler = $this->app->make(ExceptionHandlerContract::class);

            $this->app->singleton(
                ExceptionHandlerContract::class,
                function ($app) use ($appExceptionHandler) {
                    return new ExceptionHandler($app, $appExceptionHandler);
                }
            );
        }
    }

    /**
     * {@inheritdoc}
     */
    public function provides()
    {
        return [ProviderContract::class];
    }
}
