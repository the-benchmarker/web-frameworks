<?php

/**
 * This file is part of Collision.
 *
 * (c) Nuno Maduro <enunomaduro@gmail.com>
 *
 *  For the full copyright and license information, please view the LICENSE
 *  file that was distributed with this source code.
 */

namespace NunoMaduro\Collision\Contracts;

/**
 * This is the Collision Highlighter contract.
 *
 * @author Nuno Maduro <enunomaduro@gmail.com>
 */
interface Highlighter
{
    /**
     * Highlights the provided content.
     *
     * @param  string $content
     * @param  int $line
     *
     * @return string
     */
    public function highlight(string $content, int $line): string;
}
