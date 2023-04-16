<?php

use Ubiquity\controllers\Router;

/**
 * **************************
 * Ubiquity micro application
 * **************************
 */

Router::get('_default', function () {
    echo '';
});

Router::get('/user/{id}', function ($id) {
    echo $id;
});

Router::post('/user', function () {
    echo '';
});

Router::addCallableRoute('{page}', function ($page) {
    \http_response_code(404);
});
