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

namespace Silver\Core\Http;

use Silver\Http\Session;

class Lang
{

    public static function get($relativePath)
    {
        $relativePath = explode('.', $relativePath);
        
        if(Session::exists('lang')) {
            $file = include ROOT . 'Storage/Lang/'.Session::get('lang').'/'.$relativePath[0].EXT;
        } else {
            $file = include ROOT . 'Storage/Lang/en/'. $relativePath[0].EXT;
        }

        return $file[$relativePath[1]];
    }

    public static function set($name)
    {
        Session::set('lang', $name);
    }
}