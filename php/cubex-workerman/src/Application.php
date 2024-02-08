<?php

namespace App;

use Cubex\Controller\Controller;
use Symfony\Component\HttpFoundation\Response;

class Application extends Controller
{
    protected function _generateRoutes()
    {
        yield self::_route('/user/{id}', 'user');
        yield self::_route('/user', 'user');
        return 'home';
    }

    public function postUser()
    {
        return "";
    }

    public function getUser()
    {
        return Response::create($this->routeData()->getInt('id'));
    }

    public function getHome()
    {
        return "";
    }
}
