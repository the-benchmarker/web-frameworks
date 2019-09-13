<?php

/*
|--------------------------------------------------------------------------
| JSON-RPC v2.0 Compatibility Layer with 'method' member as 'class.method'
|--------------------------------------------------------------------------
*/

// route_rpc();

/*
|--------------------------------------------------------------------------
| Allow only alphanumeric and GET request characters on the request URI
|--------------------------------------------------------------------------
*/

if (isset($_SERVER[URL_PARSE]) && preg_match('/[^a-zA-Z0-9_\/?&=-]/i', $_SERVER[URL_PARSE]) ) {

    header($_SERVER["SERVER_PROTOCOL"]." 400 Bad Request");
    exit();

}

/*
|--------------------------------------------------------------------------
| Render Homepage with JSON-RPC v2.0 Compatibility Layer
|--------------------------------------------------------------------------
*/

if ( ! isset($_SERVER[URL_PARSE]) && ! isset($json_rpc['method']) ) {

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

route_class('GET', '/users', 'UserController@list');
route_class('GET', '/user/(:num)', 'UserController@view');
route_class('POST', '/user', 'UserController@add');

/*
|--------------------------------------------------------------------------
| Handle Error 404 - Page Not Found - Invalid URI
|--------------------------------------------------------------------------
|
| Invalid page includes only four (4) files: the front controller (index.php),
| config.php, functions.php and routes.php.
|
*/

if ( count(get_included_files()) == 4 ) {

	header($_SERVER["SERVER_PROTOCOL"]." 404 Not Found");
	exit();

}