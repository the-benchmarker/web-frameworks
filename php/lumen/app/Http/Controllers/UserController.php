<?php

namespace App\Http\Controllers;

use Laravel\Lumen\Routing\Controller;

class UserController extends Controller
{
    public function show(int $id)
    {
        return $id;
    }

    public function create()
    {
        return '';
    }
}
