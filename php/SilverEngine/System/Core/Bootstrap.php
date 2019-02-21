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

use Silver\Support\Log;

class Bootstrap
{
    public $url;

    function __construct()
    {
        $this->_checkURL();
        $this->url = $this->_grabURL(@$_GET['url']);
    }

    protected function _checkURL()
    {
        return (string)isset($_GET['url']) ? $_GET['url'] : false;
    }


    protected static function _grabURL($url = false)
    {
        $url = isset($_GET['url']) ? $_GET['url'] : false;

        if ($url) {
            return (array)explode('/', filter_var(rtrim($url, '/'), FILTER_SANITIZE_URL));
        }

        return (array)[];
    }
}
