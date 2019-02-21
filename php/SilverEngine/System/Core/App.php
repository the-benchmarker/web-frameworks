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

use Silver\Core\Blueprints\InstanceInterface;

class App implements InstanceInterface
{
    private static $current = null;
    private $path = 'App/';
    private $instances = [];

    public function __construct()
    {
        $this->instances = new Instances;
    }

    public function instances()
    {
        return $this->instances;
    }

    public function register(...$instances)
    {
        $last = null;
        foreach ($instances as $instance) {
            if (is_array($instance)) {
                foreach ($instance as $name => $inst) {
                    if (is_numeric($name)) {
                        $last = $this->instances->register($inst);
                    } else {
                        $last = $this->instances->registerNamed($name, $inst);
                    }
                }
            } else {
                $last = $this->instances->register($instance);
            }
        }
        return $last;
    }

    // Todoo: Facades
    public function path($path = '')
    {
        return $this->path . $path;
    }

    public function systemPath($path = '')
    {
        return 'System/App/' . $path;
    }

    public function find($path)
    {
        $target = $this->path($path);
        if (file_exists($target)) {
            return $target;
        }

        $target = $this->systemPath($path);
        if (file_exists($target)) {
            return $target;
        }

        return null;
    }

    public static function instance()
    {
        if (self::$current === null) {
            return self::$current = new static;
        } else {
            return self::$current;
        }
    }
}
