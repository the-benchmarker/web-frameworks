<?php

namespace app\controller;

use support\Request;

class User
{
    public function index(Request $request)
    {
        return '';
    }

    public function view(Request $request, $id)
    {
        return $id;
    }
}
