<?php

/**
 * The AppController is a Class Controller reserved for endpoint
 * routes, i.e. REST endpoints, using the route_class() function.
 */

class AppController
{
    public function index()
    {
        echo '';
    }

    public function viewUser()
    {
        echo url_path(2);
    }

    public function addUser()
    {
        echo '';
    }
}
