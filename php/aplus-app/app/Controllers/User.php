<?php

namespace App\Controllers;

use Framework\MVC\Controller;

final class User extends Controller
{
    public function show(int $id) : int
    {
        return $id;
    }

    public function create() : string
    {
        return '';
    }
}
