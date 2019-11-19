<?php

/*
|--------------------------------------------------------------------------
| Enforce SSL/HTTPS
|--------------------------------------------------------------------------
*/

force_ssl();

/*
|--------------------------------------------------------------------------
| Allow only URI_WHITELISTED characters on the Request URI.
|--------------------------------------------------------------------------
*/

if (! empty(URI_WHITELISTED)) {
    $regex_array = str_replace('w', 'alphanumeric', URI_WHITELISTED);
    $regex_array = explode('\\', $regex_array);

    if (isset($_SERVER['REQUEST_URI']) && preg_match('/[^' . URI_WHITELISTED . ']/i', $_SERVER['REQUEST_URI'])) {
        header($_SERVER["SERVER_PROTOCOL"]." 400 Bad Request");
        exit('<h1>The URI should only contain alphanumeric and GET request characters:</h1><h3><ul>' . implode('<li>', $regex_array) . '</ul></h3>');
    }
}

/*
|--------------------------------------------------------------------------
| Deny POST_BLACKLISTED characters in $_POST global variable array.
| Backslash (\) is blacklisted by default.
|--------------------------------------------------------------------------
*/

if (! empty(POST_BLACKLISTED)) {
    $regex_array = explode('\\', POST_BLACKLISTED);

    if (isset($_POST) && preg_match('/[' . POST_BLACKLISTED . '\\\]/i', implode('/', $_POST))) {
        header($_SERVER["SERVER_PROTOCOL"]." 400 Bad Request");
        exit('<h1>Submitted data should NOT contain the following characters:</h1><h3><ul>' . implode('<li>', $regex_array) . '<li>\</ul></h3>');
    }
}

/*
|--------------------------------------------------------------------------
| JSON-RPC v2.0 Compatibility Layer with 'method' member as 'class.method'
|--------------------------------------------------------------------------
*/

route_rpc();

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

route_auto();

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
