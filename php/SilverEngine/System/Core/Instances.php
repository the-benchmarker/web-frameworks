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

/*
 * Container for useful objects.
 */
class Instances
{
    private $container = [];

    public function register($instance, $force = false)
    {
        if (is_object($instance)) {
            $class = get_class($instance);
            return $this->registerNamed($class, $instance, $force);
        } else {
            throw new Exception("Unable to register non-object instance.");
        }
    }

    public function registerNamed($key, $value, $force = false) 
    {
        if (!isset($this->container[$key]) || $force) {
            $this->container[$key] = $value;
        } else {
            throw new Exception("Instance of $key already exists.");
        }
        return $value;
    }

    public function get($type)
    {
        if (isset($this->container[ $type ])) {
            return $this->container[ $type ];
        }

        return null;
    }

    public function getAll()
    {
        return $this->container;
    }
}