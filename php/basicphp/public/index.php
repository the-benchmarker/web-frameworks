<?php

require_once __DIR__ . '/../Basic.php'; // BasicPHP class library

/*
|--------------------------------------------------------------------------
| Routing
|--------------------------------------------------------------------------
*/

Basic::route('GET', '/', function () {
    Basic::apiResponse(200, '');
});
Basic::route('GET', '/user/(:num)', function () {
    Basic::apiResponse(200, Basic::segment(2));
});
Basic::route('POST', '/user', function () {
    Basic::apiResponse(200, '');
});

Basic::apiResponse(404); // Error 404
