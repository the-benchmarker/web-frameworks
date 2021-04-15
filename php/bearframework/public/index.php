<?php

use BearFramework\App;

require '../vendor/autoload.php';

$app = new App();

$app->routes
    ->add('/', function () {
        return new App\Response('');
    })
    ->add('/user/?', function (App\Request $request) {
        return new App\Response($request->path->getSegment(1));
    })
    ->add('POST /user', function () {
        return new App\Response('');
    });

$app->run();
