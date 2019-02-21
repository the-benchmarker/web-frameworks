<?php

/**
 * SilverEngine  - PHP MVC framework
 *
 * @package   SilverEngine
 * @author    SilverEngine Team
 * @copyright 2015-2017
 * @license   MIT
 * @link      https://github.com/SilverEngine/Framework
 */

namespace Silver\Helpers;

class String
{
    public static function endsWith($str, $end)
    {
        return strpos($str, $end) === strlen($str) - strlen($end);
    }

    public static function startsWith($str, $begin)
    {
        return strpos($str, $begin) === 0;
    }
}
