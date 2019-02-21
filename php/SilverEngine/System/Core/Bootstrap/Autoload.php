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

require_once ROOT . 'System/Core/Env.php';

use Silver\Core\Env;

spl_autoload_register(
    function ($alias) {
        $providers = Env::get('providers', []);

        $root = dirname(__FILE__) . '/';
        foreach ($providers as $prefix => $path) {
            if (strpos($alias, $prefix) == 0) {
                $class = substr($alias, strlen($prefix));
                $class_path = str_replace('\\', '/', $class) . EXT;
                $full_path = $root . $path . $class_path;

                if (file_exists($full_path)) {
                    include_once $full_path;

                    return;
                }
            }
        }
    }
);


if (file_exists($composer = ROOT . 'vendor/autoload.php')) {
    include $composer;
}
