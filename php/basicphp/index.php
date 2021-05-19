<?php

require_once __DIR__ . '/Basic.php'; // BasicPHP class library

/*
|--------------------------------------------------------------------------
| Routing
|--------------------------------------------------------------------------
*/

Basic::route('GET', '/', function() {
    echo '';
});
Basic::route('GET', '/user/(:num)', function() {
    echo Basic::segment(2);
});
Basic::route('POST', '/user', function() {
    echo '';
});

Basic::apiResponse(404); // Error 404
