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

use \stdClass;

class Env
{

    protected static $envData;
    protected static $name;

    /**
     *
     */
    public static function construct()
    {
        self::$name = $envtype = file_get_contents(ROOT . '.env');
        $envfile = ROOT . $envtype . '.env' . EXT;

        if (!is_file($envfile)) {
            exit("Env {$envtype} not found!");
        }

        $config = self::readConfiguration();
        $env = include $envfile;

        foreach ($env as $name => $value) {
            if (key_exists($name, $config)) {
                if (is_array($config[ $name ]) and is_array($value)) {
                    $config[ $name ] = self::merge($config[ $name ], $value);
                } else {
                    $config[ $name ] = $value;
                }
            } else {
                $config[ $name ] = $value;
            }
        }

        self::$envData = json_decode(json_encode($config));
    }

    /**
     * @return mixed
     */
    public static function name()
    {
        return self::$name;
    }

    /**
     * @param bool $name
     * @return object
     */
    public static function get($name, $default = null)
    {
        $data = self::$envData;
        while (true) {
            $name = explode('.', $name, 2);
            if (isset($data->{$name[0]})) {
                $data = $data->{$name[0]};
            } else {
                return $default;
            }

            if (count($name) == 1) {
                return $data;
            } else {
                $name = $name[1];
            }
        }
    }

    /**
     * @return array
     */
    private static function readConfiguration()
    {
        $config = [];
        $file_list = scandir(ROOT . 'Config/');

        foreach ($file_list as $file) {
            if ($file == '.' or $file == '..') {
                continue;
            }

            $name = strtolower(substr($file, 0, -4));
            $config[ $name ] = include ROOT . 'Config/' . $file;
        }

        return $config;
    }

    private static function merge($original, $new)
    {
        if (self::isNumericArray($original) and self::isNumericArray($new)) {
            if (isset($new[0])) {
                ksort($new);
                foreach ($new as $value) {
                    $original[] = $value;
                }
            } else {
                foreach ($new as $num => $value) {
                    $original[ $num ] = $value;
                }
            }
            ksort($original);
            return $original;
        } else {
            return array_merge($original, $new);
        }
    }

    private static function isNumericArray($array)
    {
        $numeric = true;
        foreach ($array as $key => $ignore) {
            if (!is_numeric($key)) {
                $numeric = false;
                break;
            }
        }
        return $numeric;
    }
}

Env::construct();
