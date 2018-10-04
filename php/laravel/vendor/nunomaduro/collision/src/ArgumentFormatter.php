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

use NunoMaduro\Collision\Contracts\ArgumentFormatter as ArgumentFormatterContract;

/**
 * This is an Collision Argument Formatter implementation.
 *
 * @author Nuno Maduro <enunomaduro@gmail.com>
 */
class ArgumentFormatter implements ArgumentFormatterContract
{
    /**
     * {@inheritdoc}
     */
    public function format(array $arguments, bool $recursive = true): string
    {
        $result = [];

        foreach ($arguments as $argument) {
            switch (true) {
                case is_string($argument):
                    $result[] = '"'.$argument.'"';
                    break;
                case is_array($argument):
                    $associative = array_keys($argument) !== range(0, count($argument) - 1);
                    if ($recursive && $associative && count($argument) <= 5) {
                        $result[] = '['.$this->format($argument, false).']';
                    }
                    break;
                case is_object($argument):
                    $class = get_class($argument);
                    $result[] = "Object($class)";
                    break;
            }
        }

        return implode(', ', $result);
    }
}
