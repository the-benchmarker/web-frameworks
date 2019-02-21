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

class URL
{

    private $parts = [];

    public function __construct($url)
    {
        $this->merge($url);
    }

    public static function make($url, $query = [])
    {
        $url = new static($url);
        if ($query) {
            $url->setPart('query', $query);
        }

        return $url;
    }

    public static function mail($email)
    {
        return new static('mailto', null, null, null, $email);
    }

    public function setPart($key, $value, $override = true)
    {
        if ($key == 'path') {
            $key = ($value[0] == '/') ? 'absPath' : 'relPath';
        }

        if (!isset($this->parts[ $key ]) || $override) {
            $this->parts[ $key ] = $value;
        }

        return $this;
    }

    public function getPath()
    {
        $abs = $this->part('absPath', '');
        $rel = $this->part('relPath', '');
        if ($abs && $rel && !String::endsWith($abs, '/')) {
            $abs .= '/';
        }

        return $abs . $rel;
    }

    public function part($key, $default = null)
    {
        if ($key == 'path') {
            return $this->getPath();
        }

        return isset($this->parts[ $key ])
            ? $this->parts[ $key ]
            : $default;
    }

    public function merge($url, $override = true)
    {
        foreach (parse_url($url) as $key => $value) {
            $this->setPart($key, $value, $override);
        }

        return $this;
    }


    // XXX Trait
    public function __call($method, $args)
    {
        if (String::startsWith($method, 'set')) {
            $key = lcfirst(substr($method, 3));
            $this->setParam($key, $args[0]);

            return $this;
        } elseif (String::startsWith($method, 'get')) {
            $key = lcfirst(substr($method, 3));

            return $this->part($key);
        } else {
            throw new Exception("Call undefined method " . static::class . "::" . $method . '()');
        }
    }

    public function __toString()
    {
        $url = '';

        if ($scheme = $this->part('scheme')) {
            $url .= $scheme . '://';
        }

        if ($host = $this->part('host')) {
            if ($user = $this->part('user')) {
                $url .= urlencode($user);
                if ($pass = $this->part('pass')) {
                    $url .= ':' . urlencode($pass);
                }
                $url .= '@';
            }
            $url .= urlencode($host);
        }

        if ($path = $this->part('path')) {
            $url .= implode('/', array_map('urlencode', explode('/', $path)));
        }

        if (($query = $this->part('query')) && is_array($query)) {
            $url .= '?' . http_build_query($query);
        }

        if ($fragment = $this->part('fragment')) {
            $url .= '#' . $fragment;
        }

        return $url;
    }
}