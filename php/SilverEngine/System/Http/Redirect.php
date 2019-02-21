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

use Silver\Exception;

/**
 *
 */
class Redirect
{
    public static function to($url, $permanent = false)
    {
        //return Siilver\Core\Bootstrap\Facades\Response::instance()->redirect($url);
        // FIXME: please fix the problem with redirects fix works for now.
        if (headers_sent() === false) {
            header('Location: ' . $url, true, ($permanent === true) ? 301 : 302);
        }

        exit();
    }

    public static function back($fallback = null)
    {
        if(isset($_SERVER['HTTP_REFERER'])) {
            return self::to($_SERVER['HTTP_REFERER']);
        } else {
            if ($fallback !== null) {
                return self::to($fallback);
            } else {
                throw new Exception("Unknow referer.");
            }
        }
    }
}
