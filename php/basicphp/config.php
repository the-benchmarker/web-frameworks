<?php

/*
|--------------------------------------------------------------------------
| Start Session
|--------------------------------------------------------------------------
*/

session_start();

/*
|--------------------------------------------------------------------------
| Register The Class Autoloader
|--------------------------------------------------------------------------
|
| Folders containing classes that need to be autoloaded should be added to
| the array variable $class_folders.
|
*/

// Add class folders to autoload
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
| Set The Environment
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
| Enforce SSL/HTTPS
|--------------------------------------------------------------------------
*/

define('ENFORCE_SSL', false);

/*
|--------------------------------------------------------------------------
| Set URI Whitelisted Characters
|--------------------------------------------------------------------------
*/

define('URI_WHITELISTED', '\w\/\-\?\=\&');

/*
|--------------------------------------------------------------------------
| Set $_POST Blacklisted Characters
| Backslash (\) is blacklisted by default.
|--------------------------------------------------------------------------
*/

define('POST_BLACKLISTED', '\<\>\{\}\[\]\_\;\*\=\+\"\&\#\%\\$');

/*
|--------------------------------------------------------------------------
| Set BASE_URL
|--------------------------------------------------------------------------
*/

if (ENFORCE_SSL == false) {
    $http_protocol = 'http://';
} else {
    $http_protocol = 'https://';
}
if (! empty(dirname($_SERVER['SCRIPT_NAME']))) {
    $subfolder = dirname($_SERVER['SCRIPT_NAME']);
} else {
    $subfolder = '';
}

define('BASE_URL', $http_protocol . $_SERVER['SERVER_NAME'] . $subfolder . '/');

/*
|--------------------------------------------------------------------------
| Number of subdirectories from hostname to index.php
|--------------------------------------------------------------------------
*/

define('SUB_DIR', substr_count($_SERVER['SCRIPT_NAME'], '/')-1);

/*
|--------------------------------------------------------------------------
| Set Homepage Controller@method
|--------------------------------------------------------------------------
*/

define('HOME_PAGE', 'AppController@index');

/*
|--------------------------------------------------------------------------
| Set Controller Suffix
|--------------------------------------------------------------------------
|
| If you are using 'ClassController' convention, set to 'Controller'.
|
*/

define('CONTROLLER_SUFFIX', 'Controller');

/*
|--------------------------------------------------------------------------
| Set Default Method
|--------------------------------------------------------------------------
|
| If the second URL string is empty, set this method as the default method.
|
*/

define('METHOD_DEFAULT', 'index');
