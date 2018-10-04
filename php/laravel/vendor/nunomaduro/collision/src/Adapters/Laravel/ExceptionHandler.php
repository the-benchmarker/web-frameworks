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

use Exception;
use Illuminate\Contracts\Foundation\Application;
use NunoMaduro\Collision\Contracts\Provider as ProviderContract;
use Illuminate\Contracts\Debug\ExceptionHandler as ExceptionHandlerContract;
use Symfony\Component\Console\Exception\ExceptionInterface as SymfonyConsoleExceptionInterface;

/**
 * This is an Collision Laravel Adapter ExceptionHandler implementation.
 *
 * Registers the Error Handler on Laravel.
 *
 * @author Nuno Maduro <enunomaduro@gmail.com>
 */
class ExceptionHandler implements ExceptionHandlerContract
{
    /**
     * Holds an instance of the application exception handler.
     *
     * @var \Illuminate\Contracts\Debug\ExceptionHandler
     */
    protected $appExceptionHandler;

    /**
     * Holds an instance of the application.
     *
     * @var \Illuminate\Contracts\Foundation\Application
     */
    protected $app;

    /**
     * Creates a new instance of the ExceptionHandler.
     *
     * @param \Illuminate\Contracts\Foundation\Application $app
     * @param \Illuminate\Contracts\Debug\ExceptionHandler $appExceptionHandler
     */
    public function __construct(Application $app, ExceptionHandlerContract $appExceptionHandler)
    {
        $this->app = $app;
        $this->appExceptionHandler = $appExceptionHandler;
    }

    /**
     * {@inheritdoc}
     */
    public function report(Exception $e)
    {
        $this->appExceptionHandler->report($e);
    }

    /**
     * {@inheritdoc}
     */
    public function render($request, Exception $e)
    {
        return $this->appExceptionHandler->render($request, $e);
    }

    /**
     * {@inheritdoc}
     */
    public function renderForConsole($output, Exception $e)
    {
        if ($e instanceof SymfonyConsoleExceptionInterface) {
            $this->appExceptionHandler->renderForConsole($output, $e);
        } else {
            $handler = $this->app->make(ProviderContract::class)
                ->register()
                ->getHandler()
                ->setOutput($output);

            $handler->setInspector((new Inspector($e)));

            $handler->handle();
        }
    }

    /**
     * Determine if the exception should be reported.
     *
     * @param  \Exception  $e
     * @return bool
     */
    public function shouldReport(Exception $e)
    {
        return $this->appExceptionHandler->shouldReport($e);
    }
}
