<?php

namespace App\Providers;

use MacropaySolutions\Framework\Providers\EventServiceProvider as ServiceProvider;

class EventServiceProvider extends ServiceProvider
{
    /**
     * The event listener mappings for the application.
     *
     * @var array
     */
    protected $listen = [
//        \App\Events\ExampleEvent::class => [
//            \App\Listeners\ExampleListener::class,
//        ],
    ];

    /**
     * Determine if events and listeners should be automatically discovered.
     */
    public function shouldDiscoverEvents(): bool
    {
        return false;
    }

    /**
     * Determine if events as observers and listeners should be automatically discovered.
     */
    public function shouldDiscoverEventsAsObservers(): bool
    {
        return true;
    }
}
