<?php

require_once 'vendor/autoload.php';

use laylatichy\Nano;
use laylatichy\nano\core\HttpCode;

$nano = new Nano();

$nano->get(
    '/',
    function ($request) use ($nano) {
        $nano->response->code(code: HttpCode::OK);
        $nano->response->plain(body: '');
    }
);

$nano->get(
    '/user/{id}',
    function ($request, $id) use ($nano) {
        $nano->response->code(code: HttpCode::OK);
        $nano->response->plain(body: $id);
    }
);

$nano->post(
    '/user',
    function ($request) use ($nano) {
        $nano->response->code(code: HttpCode::OK);
        $nano->response->plain(body: '');
    }
);

$nano->start();
