<?php

require '../vendor/autoload.php';

Flight::route(
    'GET /',
    function () {
        echo '';
    }
);

Flight::route(
    'GET /user/@id',
    function ($id) {
        echo $id;
    }
);

Flight::route(
    'POST /user',
    function () {
        echo '';
    }
);

Flight::start();
