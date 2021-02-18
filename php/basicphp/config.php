<?php

/*
|--------------------------------------------------------------------------
| Session
|--------------------------------------------------------------------------
*/

// session_start(); // start session

/*
|--------------------------------------------------------------------------
| Class Autoloader
|--------------------------------------------------------------------------
|
| Folders containing classes that need to be autoloaded should be added to
| the array variable $class_folders.
|
*/

// Autoload classes in folders
$class_folders = []; // set as an empty array
$class_folders[] = 'classes';
$class_folders[] = 'models';
$class_folders[] = 'controllers';

define('AUTOLOAD_CLASSES', $class_folders);

spl_autoload_register(function ($class_name) {

	foreach (AUTOLOAD_CLASSES as $folder) {
		if (file_exists('../' . $folder . '/' . $class_name . '.php') && is_readable('../' . $folder . '/' . $class_name . '.php')) {
			require_once '../' . $folder . '/' . $class_name . '.php';
		}
	}

});

/*
|--------------------------------------------------------------------------
| Environment
|--------------------------------------------------------------------------
|
| When working in a development environment, define 'ENVIRONMENT' as
| 'development'. When working in a production environment, define
| 'ENVIRONMENT' as 'production'. Error reporting is turned ON in development,
| and OFF in production environment.
|
*/

define('ENVIRONMENT', 'development');

switch (ENVIRONMENT) {
    case 'development':
        error_reporting(E_ALL);
        break;
    case 'production':
        error_reporting(0);
        break;
}

/*
|--------------------------------------------------------------------------
| Firewall Settings
|--------------------------------------------------------------------------
*/

// Turn firewall ON or OFF as TRUE or FALSE.
define('FIREWALL_ON', FALSE);
// List of allowed IP addresses in an array
define('ALLOWED_IP_ADDR', ['::1']);
// URI whitelisted characters in regular expression
define('URI_WHITELISTED', '\w\/\.\-\_\?\=\&');
// Blacklisted $_POST and post body characters in regular expression
// Backslash (\) is blacklisted by default.
define('POST_BLACKLISTED', '\<\>\;\#\\$');

/*
|--------------------------------------------------------------------------
| SSL/HTTPS
|--------------------------------------------------------------------------
*/

// Set to TRUE to enforce SSL/HTTPS
define('ENFORCE_SSL', FALSE);

/*
|--------------------------------------------------------------------------
| Encryption and Decryption
|--------------------------------------------------------------------------
*/

// Passphrase for key derivation
define('PASS_PHRASE', '12345');
// Cipher method
define('CIPHER_METHOD', 'aes-256-ctr');

// Limit to AES mode: CBC, CTR or GCM
if (! in_array(CIPHER_METHOD, ['aes-256-cbc', 'aes-256-ctr', 'aes-256-gcm'])) {
    exit('<strong>Warning: </strong>Only CBC, CTR and GCM modes of AES are supported.');
}

/*
|--------------------------------------------------------------------------
| BASE_URL
|--------------------------------------------------------------------------
*/

$http_protocol = (ENFORCE_SSL == FALSE) ? 'http://' : 'https://';
$subfolder = (! empty(dirname($_SERVER['SCRIPT_NAME']))) ? dirname($_SERVER['SCRIPT_NAME']) : '';

define('BASE_URL', $http_protocol . $_SERVER['SERVER_NAME'] . $subfolder . '/');

/*
|--------------------------------------------------------------------------
| Default Controller Suffix and Method
|--------------------------------------------------------------------------
|
| If using 'ClassController' convention, set suffix to 'Controller'.
| If method index() is the default class callable, set method to 'index'.
|
*/

define('CONTROLLER_SUFFIX', 'Controller');
define('METHOD_DEFAULT', 'index');

/*
|--------------------------------------------------------------------------
| Homepage Callable - Controller@method
|--------------------------------------------------------------------------
*/

define('HOME_PAGE', '');