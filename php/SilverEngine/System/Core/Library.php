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


use Silver\Helpers\URL;
use Silver\Helpers\HTMLElement as El;
use Silver\Core\Route;
use Silver\Http\View;

/**
 * @param $data
 * @param bool $param
 */
if (!function_exists('dd')) {
    function dd($data, $param = false)
    {
        if ($param) {
            var_dump('<pre>', $data, '<pre>');
        } else {
            var_dump($data);
        }

        exit();
    }
}


/**
 * @param $data
 * @param bool $param
 */
if (!function_exists('ndd')) {
    function ndd($data, $param = false)
    {
        if ($param) {
            var_dump('<pre>', $data, '<pre>');
        } else {
            var_dump($data);
        }
    }
}

/**
 * @param $data
 * @param bool $param
 */
if (!function_exists('debug')) {
    function debug($data, $param = false)
    {
        if ($param) {
            var_dump($data);
        } else {
            var_dump('<pre>', $data, '<pre>');
        }
    }
}

/**
 * @param $name
 */
if (!function_exists('view')) {
    function view($name, $data = [])
    {
        return View::make($name, $data);
    }
}

if (!function_exists('url')) {
    function url($path = '/', $query = [])
    {
        if ($path[0] == '/') {
            $url = URL::make($path, $query);
            $url->merge(URL . $url->part('absPath'));
            return $url;
        } else {
            $url = URL::make(CURRENT_URL, $query);
            $url->merge($path);
            return $url;
        }
    }
}

if (!function_exists('asset')) {
    function asset($url, $query = [])
    {
        $url = URL::make($url, $query);
        $url->merge(URL . '/public' . $url->part('absPath', ''));
        return $url;
    }
}

if (!function_exists('alink')) {
    function alink($text, $url, $attrs = [])
    {
        $el = new El(
            'a', array_merge(
                [
                'href' => $url,
                ], $attrs
            )
        );

        return $el->appendChild($text ?: $url);
    }
}

if (!function_exists('css')) {
    function css($url, $attrs = [])
    {
        return new El(
            'link', array_merge(
                [
                'type' => 'text/css',
                'href' => $url,
                ], $attrs
            )
        );
    }
}

if (!function_exists('js')) {
    function js($url, $attrs = [])
    {
        return new El(
            'script', array_merge(
                [
                'type' => 'text/javascript',
                'src'  => (string)$url,
                ], $attrs
            )
        );
    }
}


if (!function_exists('route')) {
    function route($name, $args = [])
    {
        //TODO: catch error if route not exists,
        return Route::getRoute($name)->url($args);
    }
}

if(!function_exists('mapArrayKeys')) {
    function mapArrayKeys($array,callable $callback)
    {
        $arr = array();

        foreach($array as $key => $value)
        {
            $arr[$callback($key)] = $value;
        }
        return $arr;
    }
}
