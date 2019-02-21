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


error_reporting(E_ALL);
ini_set('display_errors', 'On');
session_start();


/**
 * Defining base url
 * and base path.
 */
$PREFIX = preg_replace('{/index.php$}', '', str_replace('\\', '/', $_SERVER['SCRIPT_NAME']));
$HOST = 'http' . (isset($_SERVER['HTTPS']) ? 's' : '')
    . '://' . $_SERVER['SERVER_NAME']
    . ($_SERVER['SERVER_PORT'] != 80 ? ':' . $_SERVER['SERVER_PORT'] : '');

// directory separator to prevent conflict in different OS
define('DS', DIRECTORY_SEPARATOR);

define('BASEPATH', $PREFIX);
define('URL', $HOST . $PREFIX); // BASE_URL
define('CURRENT_URL', $HOST . $_SERVER['REQUEST_URI']); // URL

// ROOT DIR
define('ROOT', dirname(dirname(__DIR__)).DS);
// System DIR
define('SYS', ROOT . 'System' .DS);
define('CORE', SYS . 'Core' .DS);



/**
 * Defining php extension.
 */
define('EXT', '.php');

// files to be included
require_once CORE . 'ErrorHandlers.php';
require_once CORE . 'Library.php';

