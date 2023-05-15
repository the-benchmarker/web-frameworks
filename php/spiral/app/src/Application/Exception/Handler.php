<?php

declare(strict_types=1);

namespace App\Application\Exception;

use Spiral\Exceptions\ExceptionHandler;

/**
 * In this class you can override default exception handler behavior,
 * or add custom renderers or reporters.
 */
final class Handler extends ExceptionHandler
{
    protected function bootBasicHandlers(): void
    {
        parent::bootBasicHandlers();
        // $this->addRenderer(new CustomRenderer());
        // $this->addReporter(new CustomReporter());
    }
}
