<?php

/**
 * This file is part of Collision.
 *
 * (c) Nuno Maduro <enunomaduro@gmail.com>
 *
 *  For the full copyright and license information, please view the LICENSE
 *  file that was distributed with this source code.
 */

namespace NunoMaduro\Collision;

use Whoops\Run;
use Whoops\RunInterface;
use NunoMaduro\Collision\Contracts\Handler as HandlerContract;
use NunoMaduro\Collision\Contracts\Provider as ProviderContract;

/**
 * This is an Collision Provider implementation.
 *
 * @author Nuno Maduro <enunomaduro@gmail.com>
 */
class Provider implements ProviderContract
{
    /**
     * Holds an instance of the Run.
     *
     * @var \Whoops\RunInterface
     */
    protected $run;

    /**
     * Holds an instance of the handler.
     *
     * @var \NunoMaduro\Collision\Contracts\Handler
     */
    protected $handler;

    /**
     * Creates a new instance of the Provider.
     *
     * @param \Whoops\RunInterface|null $run
     * @param \NunoMaduro\Collision\Contracts\Handler|null $handler
     */
    public function __construct(RunInterface $run = null, HandlerContract $handler = null)
    {
        $this->run = $run ?: new Run;
        $this->handler = $handler ?: new Handler;
    }

    /**
     * {@inheritdoc}
     */
    public function register(): ProviderContract
    {
        $this->run->pushHandler($this->handler)
            ->register();

        return $this;
    }

    /**
     * {@inheritdoc}
     */
    public function getHandler(): HandlerContract
    {
        return $this->handler;
    }
}
