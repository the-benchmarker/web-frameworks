<?php

use Phalcon\Mvc\Micro;

$app = new Micro();

$app->get(
    '/',
    function () use ($app) {
        $response = $app->response;
        $response->setStatusCode(200, "OK");
        $response->setContent("");

        return $response;
    }
);

$app->get(
    '/user/{id}',
    function ($id) use ($app) {
        $response = $app->response;
        $response->setStatusCode(200, "OK");
        $response->setContent($id);

        return $response;
    }
);

$app->post(
    '/user',
    function () use ($app) {
        $response = $app->response;
        $response->setStatusCode(200, "OK");
        $response->setContent("");

        return $response;
    }
);


$app->handle(
    $_SERVER["REQUEST_URI"]
);
