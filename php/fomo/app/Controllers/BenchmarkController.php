<?php

namespace App\Controllers;

use Fomo\Request\Request;
use Fomo\Response\Response;

class BenchmarkController
{
    public function index(Request $request): Response
    {
        return response()->asNoContent();
    }

    public function getUser(Request $request, $id): Response
    {
        return response(
            $id, 200);
    }

    public function postUser(): Response 
    {
        return response()->asNoContent();
    }
}
