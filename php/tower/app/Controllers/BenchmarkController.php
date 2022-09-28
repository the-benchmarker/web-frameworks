<?php 

namespace App\Controllers;

use Tower\Request;

class BenchmarkController
{
    public function index(): string
    {
        return '';
	}

    public function getUser(Request $request , $user)
    {
        return $user;
	}

	public function postUser(): string
    {
        return '';
	}
}