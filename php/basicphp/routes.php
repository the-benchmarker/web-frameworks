<?php

/*
|--------------------------------------------------------------------------
| Enforce SSL/HTTPS
|--------------------------------------------------------------------------
*/

// if ( ENFORCE_SSL == TRUE && ( ! isset($_SERVER['HTTPS']) || $_SERVER['HTTPS'] !== 'on' ) ) {
// 	header('Location: https://' . $_SERVER['SERVER_NAME'] . $_SERVER['REQUEST_URI']);
// 	exit();
// }

/*
|--------------------------------------------------------------------------
| Allow only alphanumeric and GET request characters on the Request URI.
|--------------------------------------------------------------------------
*/

<<<<<<< HEAD
// $regex_whitelist = "\w\/\-\?\=\&";

// $regex_array = str_replace('w', 'alphanumeric', $regex_whitelist);
// $regex_array = explode('\\', $regex_array);

// if (isset($_SERVER['REQUEST_URI']) && preg_match('/[^' . $regex_whitelist . ']/i', $_SERVER['REQUEST_URI'])) {

// 	header($_SERVER["SERVER_PROTOCOL"]." 400 Bad Request");
// 	exit('<h1>The URI should only contain alphanumeric and GET request characters:</h1><h3><ul>' . implode('<li>', $regex_array) . '</ul></h3>');
	
// }

/*
|--------------------------------------------------------------------------
| Deny input with blacklisted characters in $_POST global variable array.
|--------------------------------------------------------------------------
*/

// $regex_blacklist = "\<\>\{\}\[\]\_\;\*\=\+\'\&\#\%\\$";

// $regex_array = explode('\\', $regex_blacklist);

// if (isset($_POST) && preg_match('/[' . $regex_blacklist . '\"\\\]/i', implode('/', $_POST)) ) {

// 	header($_SERVER["SERVER_PROTOCOL"]." 400 Bad Request");
// 	exit('<h1>Submitted data should NOT contain the following characters:</h1><h3><ul>' . implode('<li>', $regex_array) . '<li>"<li>\</ul></h3>');
	
// }

/*
|--------------------------------------------------------------------------
| JSON-RPC v2.0 Compatibility Layer with 'method' member as 'class.method'
|--------------------------------------------------------------------------
*/

// route_rpc();
=======
if (isset($_SERVER[URL_PARSE]) && preg_match('/[^a-zA-Z0-9_\/?&=-]/i', $_SERVER[URL_PARSE])) {
    header($_SERVER["SERVER_PROTOCOL"]." 400 Bad Request");
    exit();
}
>>>>>>> Avoid deps for nano frameworks (#1789)

/*
|--------------------------------------------------------------------------
| Render Homepage with JSON-RPC v2.0 Compatibility Layer
|--------------------------------------------------------------------------
*/

if (empty(url_value(1)) && ! isset($json_rpc['method'])) {
    list($class, $method) = explode('@', HOME_PAGE);
    $object = new $class();
    return $object->$method();
}

/*
|--------------------------------------------------------------------------
| Automatic Routing of url_value(1) and (2) as '/class/method' path
|--------------------------------------------------------------------------
*/

// route_auto();

/*
|--------------------------------------------------------------------------
| Manual Routing Using Endpoints and Wildcards to Controllers
|--------------------------------------------------------------------------
*/

route_class('GET', '/', 'AppController@index');
route_class('GET', '/user/(:num)', 'AppController@viewUser');
route_class('POST', '/user', 'AppController@addUser');

/*
|--------------------------------------------------------------------------
| Handle Error 404 - Page Not Found - Invalid URI
|--------------------------------------------------------------------------
|
| Invalid page includes only four (4) files: the front controller (index.php),
| config.php, functions.php and routes.php.
|
*/

if (count(get_included_files()) == 4) {
    header($_SERVER["SERVER_PROTOCOL"]." 404 Not Found");
    exit();
}
