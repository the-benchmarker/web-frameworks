<?php

/*
 * This file is part of Collision.
 *
 * (c) Nuno Maduro <enunomaduro@gmail.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace NunoMaduro\Collision\Contracts;

/**
 * This is an Collision Provider contract.
 *
 * @author Nuno Maduro <enunomaduro@gmail.com>
 */
interface Provider
{
    /**
     * Registers the current Handler as Error Handler.
     *
     * @return \NunoMaduro\Collision\Contracts\Provider
     */
    public function register(): Provider;

    /**
     * Returns the handler.
     *
     * @return \NunoMaduro\Collision\Contracts\Handler
     */
    public function getHandler(): Handler;
}
