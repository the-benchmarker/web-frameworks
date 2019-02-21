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

use Silver\Exception;

class Path
{
    const SEP = DIRECTORY_SEPARATOR;
    private $jail;
    private $path;

    /**
     * Make new path.
     * If you define $jail path, result path will be in it.
     *
     * You cant make absolute path outside jail.
     * new Path('/Smith', '/users') -> /users/Smith
     *
     * Class is immutable.
     */
    public function __construct($path, $jail = '') 
    {
        $this->path = '';
        if ($jail) {
            $this->setJail($jail);
        }
        $this->setPath($path);
    }

    public function cd($path) 
    {
        if ($path) {
            return new static($this->getPath() . $path, $this->getJail());
        } else {
            return $this;
        }
    }

    public function path() 
    {
        return $this->real($this->path, $this->jail, true);
    }

    public function __toString() 
    {
        return $this->path();
    }

    public function file($file) 
    {
        $dest = $this->real($file, $this->path(), true);

        if (!is_file($dest)) {
            throw new Exception("Destination is not a file.");
        }

        return $dest;
    }

    public function getPath() 
    {
        if ($this->path) {
            return $this->path;
        }
        return '';
    }

    public function getJail() 
    {
        if ($this->jail) {
            return $this->jail;
        }
        return '';
    }

    private function setPath($path) 
    {
        $path = trim($path);
        if ($path) {
            if ($path[0] != self::SEP) {
                $path = $this->getPath() . $path;
            }
            $this->path = $this->real($path, $this->getJail());
        }
    }

    private function setJail($jail) 
    {
        $jail = trim($jail);
        if ($jail) {
            $this->jail = $this->real($jail);
        }
    }

    /**
     * Return filtered path and throw exception if it is not exists.
     */
    private function real($path, $jail = '', $fullpath = false) 
    {
        if ($jail) {
            $jail = $this->real($jail);
        }

        $path = realpath($jail . '' . $path);

        if ($path === false) {
            throw new Exception("Non-existing path: $path");
        }

        if (is_dir($path)) {
            $path .= self::SEP;
        }

        if ($jail) {
            if (String::startsWith($path, $jail)) {
                if (!$fullpath) {
                    $path = substr($path, strlen($jail));
                }
            } else {
                throw new Exception("Path is outside of jail.");
            }
        }

        return $path;
    }
}