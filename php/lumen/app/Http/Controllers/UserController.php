<?php

namespace App\Http\Controllers;

use Laravel\Lumen\Routing\Controller;

class UserController extends Controller
{
    public function show($id)
    {
        return $id;
    }

    public function create()
    {
        return '';
    }
}
