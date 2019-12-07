<?php

namespace App\services;

use SwFwLess\components\http\Response;
use SwFwLess\services\BaseService;

class TestService extends BaseService
{
    public function index()
    {
        return Response::output('');
    }
    public function get(int $id)
    {
        return Response::output((string) $id);
    }
    public function create()
    {
        return Response::output('');
    }
}
