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

namespace Silver\Http;

/**
 * Session
 */
class Session
{

    public static function construct()
    {
        if (!isset($_SESSION['data'])) {
            $_SESSION['data'] = [];
        }

        if (!isset($_SESSION['old-flash'])) {
            $_SESSION['old-flash'] = [];
        }

        if (!isset($_SESSION['flash'])) {
            $_SESSION['flash'] = [];
        }

        // flash -> old-flash
        $_SESSION['old-flash'] = $_SESSION['flash'];
        $_SESSION['flash'] = [];
    }

    /*
     * Return all user data;
     */
    public static function all()
    {
        return array_merge($_SESSION['data'], $_SESSION['old-flash'], $_SESSION['flash']);
    }

    public static function set($key, $value)
    {
        self::delete($key);
        return $_SESSION['data'][$key] = $value;
    }

    public static function flash($key, $value)
    {
        self::delete($key);
        return $_SESSION['flash'][$key] = $value;
    }

    public static function exists($name)
    {
        return isset(self::all()[$name]);
    }

    public static function get($name, $default = null)
    {

        if (self::exists($name)) {
            return self::all()[$name];
        }
        return $default;
    }

    public static function delete($key)
    {
        if (isset($_SESSION['data'][$key])) {
            unset($_SESSION['data'][$key]);
        } elseif (isset($_SESSION['old-flash'][$key])) {
            unset($_SESSION['old-flash'][$key]);
        } elseif (isset($_SESSION['flash'][$key])) {
            unset($_SESSION['flash'][$key]);
        }
    }

    public static function flush()
    {
        $_SESSION = [];
    }

    public static function kill()
    {
        return session_destroy();
    }
}

Session::construct();
