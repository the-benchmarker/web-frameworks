<?php

/*
|--------------------------------------------------------------------------
| Start Session
|--------------------------------------------------------------------------
*/

// session_start();

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
// $class_folders[] = 'classes';
// $class_folders[] = 'models';
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
| Set BASE_URL
|--------------------------------------------------------------------------
|
| Define 'BASE_URL' as the domain with '/' at the end, such as
| 'http://example.com/' or 'https://example.com/'.
| Include subdirectory if index.php is not in DocumentRoot folder.
|
*/

define('BASE_URL', 'http://localhost/');

/*
|--------------------------------------------------------------------------
| Set URL_PARSE Method as either 'REQUEST_URI' or 'PATH_INFO'.
| When using Nginx server, 'REQUEST_URI' is recommended.
| SUB_DIR as the number of subfolders index.php is located from domain.
|--------------------------------------------------------------------------
|
| Sets the $_SERVER[''] global variable to parse the URL.
|
*/

define('URL_PARSE', 'REQUEST_URI');

if (URL_PARSE == 'PATH_INFO') {
    define('SUB_DIR', 0);
} elseif (URL_PARSE == 'REQUEST_URI') {
    define('SUB_DIR', substr_count(BASE_URL, '/')-3);
}

/*
|--------------------------------------------------------------------------
| Set Homepage
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
*/

define('METHOD_DEFAULT', 'index');
