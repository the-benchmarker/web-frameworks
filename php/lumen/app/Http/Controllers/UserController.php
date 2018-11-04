<?php

namespace App\Http\Controllers;

use Laravel\Lumen\Routing\Controller;

class UserController extends Controller
{
    public function show(int $userId)
    {
        return $userId;
    }

    public function create()
    {
        return '';
    }
}
