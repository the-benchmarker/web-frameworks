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

class Config
{

    public static function get($name)
    {
        if (is_file(ROOT . "/Config/{$name}.php")) {
            $config[ $name ] = (object)include ROOT . 'Config/' . $name . EXT;
        }

        return (object)$config;
    }

    public static function app()
    {
        if (is_file(ROOT . "/Config/App.php")) {
            $config['App'] = (object)include ROOT . 'Config/App' . EXT;
        }

        return (object)$config['App'];
    }
}

