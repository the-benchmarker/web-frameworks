<?php

namespace App\Controllers;

use Silver\Core\Controller;

class UserController extends Controller
{

    public function get($id)
    {
	    return $id;
    }

    public function create()
    {
	    return "";
    }
}
