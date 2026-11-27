<?php

namespace App\Jobs;

use MacropaySolutions\Kernel\Bus\InstanceDispatchable;
use MacropaySolutions\Kernel\Bus\Queueable;
use MacropaySolutions\Kernel\Contracts\Queue\ShouldQueue;
use MacropaySolutions\Kernel\Queue\InteractsWithQueue;

abstract class Job implements ShouldQueue
{
    /*
    |--------------------------------------------------------------------------
    | Queueable Jobs
    |--------------------------------------------------------------------------
    |
    | This job base class provides a central location to place any logic that
    | is shared across all of your jobs. The trait included with the class
    | provides access to the "queueOn" and "delay" queue helper methods.
    |
    */
    use InstanceDispatchable;
    use InteractsWithQueue;
    use Queueable;
}
