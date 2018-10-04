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
 * This is an Collision Argument Formatter contract.
 *
 * @author Nuno Maduro <enunomaduro@gmail.com>
 */
interface ArgumentFormatter
{
    /**
     * Formats the provided array of arguments into
     * an understandable description.
     *
     * @param  array $arguments
     * @param  bool $recursive
     *
     * @return string
     */
    public function format(array $arguments, bool $recursive = true): string;
}
