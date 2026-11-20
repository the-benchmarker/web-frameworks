<?php

namespace App\Controllers;

use Fomo\Request\Request;

class BenchmarkController
{
    public function index(): string
    {
        return response()->noContent();
    }

    public function getUser(Request $request, $id): string
    {
        return response()->html($id);
    }

    public function postUser(): string
    {
        return response()->noContent();
    }
}

