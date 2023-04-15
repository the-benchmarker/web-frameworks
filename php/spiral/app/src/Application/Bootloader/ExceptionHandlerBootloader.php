<?php

declare(strict_types=1);

namespace App\Application\Bootloader;

use App\Application\Exception\Renderer\ViewRenderer;
use Spiral\Boot\AbstractKernel;
use Spiral\Boot\Bootloader\Bootloader;
use Spiral\Exceptions\ExceptionHandler;
use Spiral\Exceptions\Renderer\ConsoleRenderer;
use Spiral\Exceptions\Renderer\JsonRenderer;
use Spiral\Exceptions\Reporter\FileReporter;
use Spiral\Exceptions\Reporter\LoggerReporter;
use Spiral\Http\ErrorHandler\RendererInterface;
use Spiral\Http\Middleware\ErrorHandlerMiddleware\EnvSuppressErrors;
use Spiral\Http\Middleware\ErrorHandlerMiddleware\SuppressErrorsInterface;

/**
 * The exception handler bootloader is responsible for registering the exception renderers and reporters.
 *
 * @link https://spiral.dev/docs/basics-errors
 */
final class ExceptionHandlerBootloader extends Bootloader
{
    protected const BINDINGS = [
        SuppressErrorsInterface::class => EnvSuppressErrors::class,
        RendererInterface::class => ViewRenderer::class,
    ];

    public function __construct(
        private readonly ExceptionHandler $handler,
    ) {
    }

    public function init(AbstractKernel $kernel): void
    {
        // Register the console renderer, that will be used when the application
        // is running in the console.
        $this->handler->addRenderer(new ConsoleRenderer());

        $kernel->running(function (): void {
            // Register the JSON renderer, that will be used when the application is
            // running in the HTTP context and a JSON response is expected.
            $this->handler->addRenderer(new JsonRenderer());
        });
    }

    public function boot(LoggerReporter $logger, FileReporter $files): void
    {
        // Register the logger reporter, that will be used to log the exceptions using
        // the logger component.
        $this->handler->addReporter($logger);

        // Register the file reporter. It allows you to save detailed information about an exception to a file
        // known as snapshot.
        $this->handler->addReporter($files);
    }
}
