<?php

require_once 'vendor/autoload.php';

use laylatichy\nano\core\httpcode\HttpCode;
use laylatichy\nano\core\router\BASE;
use laylatichy\nano\core\router\GET;
use laylatichy\nano\core\router\POST;
use laylatichy\nano\core\router\NanoRouter;
use laylatichy\nano\Nano;

$nano = new Nano();

#[BASE('')]
final class TheBenchmarker extends NanoRouter
{
    #[GET('/')]
    public function index(Nano $nano): void
    {
        $nano->response->code(HttpCode::OK);
        $nano->response->plain('');
    }

    #[GET('/user/{id}')]
    public function userId(Nano $nano, string $id): void
    {
        $nano->response->code(HttpCode::OK);
        $nano->response->plain($id);
    }

    #[POST('/user')]
    public function user(Nano $nano): void
    {
        $nano->response->code(HttpCode::OK);
        $nano->response->plain('');
    }
}

$nano->start();
