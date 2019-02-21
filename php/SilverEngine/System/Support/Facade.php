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

namespace Silver\Support;

class Facade
{
    private static $objects = [];

    public static function __callStatic($fname, $args)
    {
        $child = get_called_class();
        //        var_dump($child);
        $class = $child::getClass();
        $object = null;

        if (isset(self::$objects[$class])) {
            $object = self::$objects[$class];
        } else {
            $object = self::$objects[$class] = new $class;
        }

        return call_user_func_array(array($object, $fname), $args);
    }

}