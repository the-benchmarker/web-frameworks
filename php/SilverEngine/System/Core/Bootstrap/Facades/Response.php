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

namespace Silver\Core\Bootstrap\Facades;

use Silver\Support\Facade;

class Response extends Facade
{
    protected static function getClass()
    {
        return 'Silver\Http\Response';
    }
}