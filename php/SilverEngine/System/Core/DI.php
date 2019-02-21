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

namespace Silver\Core;

use Silver\Exception;

class DI
{
    public static function call($callable, $vars = [])
    {
        $vars = self::prepareVars($vars);

        $parameters = null;
        if (is_array($callable)) {
            list ($obj, $method) = $callable;
            $refMethod = new \ReflectionMethod(get_class($obj), $method);
            $parameters = $refMethod->getParameters();
        } else {
            $refFn = new \ReflectionFunction($callable);
            $parameters = $refFn->getParameters();
        }

        $args = [];
        foreach ($parameters as $param) {
            $name = $param->getClass()
                  ? $param->getClass()->name
                  : $param->getName();

            if (key_exists($name, $vars)) {
                $args[] = $vars[ $name ];
            } elseif ($param->isOptional()) {
                $args[] = $param->getDefaultValue();
            } else {
                throw new Exception("DI: Unable to inject parameter $name");
            }
        }

        if (is_array($callable)) {
            list ($obj, $method) = $callable;

            return $obj->$method(...$args);
        } elseif (is_callable($callable)) {
            return $callable(...$args);
        } else {
            throw new Exception("DI: Function is not callable: $callable");
        }
    }

    // in:  [$class, $class2, 'var' => 24]
    // out: ['\A\Class' => $class, '\B\Class' => $class2, 'var' => 24]
    private static function prepareVars($vars)
    {
        $vars2 = [];
        foreach ($vars as $key => $value) {
            if (is_numeric($key)) {
                if (is_object($value)) {
                    $key = '\\' . get_class($value);
                    $vars2[ $key ] = $value;
                } else {
                    throw new Exception("DI: Non object value must have specified name.");
                }
            } else {
                $vars2[ $key ] = $value;
            }
        }

        return $vars2;
    }
}