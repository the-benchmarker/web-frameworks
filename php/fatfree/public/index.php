<?php

require '../vendor/autoload.php';

$f3 = Base::instance();

$f3->route(
    'GET /',
    function () {
        echo '';
    }
);

$f3->route(
    'GET /user/@id',
    function ($f3) {
        echo $f3->get('PARAMS.id');
    }
);

$f3->route(
    'POST /user',
    function () {
        echo '';
    }
);

$f3->run();
