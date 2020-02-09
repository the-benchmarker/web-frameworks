<?php

/*
|--------------------------------------------------------------------------
| Load BasicPHP Functions Library and Configuration File
|--------------------------------------------------------------------------
*/

require_once 'functions.php';
require_once 'config.php';

/*
|--------------------------------------------------------------------------
| Security
|--------------------------------------------------------------------------
*/

// firewall(); // Firewall
// force_ssl(); // SSL/HTTPS

/*
|--------------------------------------------------------------------------
| Routing
|--------------------------------------------------------------------------
*/

// route_rpc(); // JSON-RPC v2.0
// route_auto(); // Automatic '/class/method' routing
// homepage(); // Render homepage

/*
|--------------------------------------------------------------------------
| Endpoint Routing
|--------------------------------------------------------------------------
*/

route_class('GET', '/', 'AppController@index');
route_class('GET', '/user/(:num)', 'AppController@viewUser');
route_class('POST', '/user', 'AppController@addUser');

/*
|--------------------------------------------------------------------------
| Handle Error 404 - Page Not Found - Invalid URI
|--------------------------------------------------------------------------
*/

error404(); // Handle Error 404
