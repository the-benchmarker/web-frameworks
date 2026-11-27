<?php

namespace App\Listeners;

use App\Events\ExampleEvent;
use MacropaySolutions\Kernel\Contracts\Queue\ShouldQueue;
use MacropaySolutions\Kernel\Queue\InteractsWithQueue;

class ExampleListener
{
    /**
     * Create the event listener.
     */
    public function __construct()
    {
        //
    }

    /**
     * Handle the event.
     */
    public function handle(ExampleEvent $event): void
    {
        //
    }
}
