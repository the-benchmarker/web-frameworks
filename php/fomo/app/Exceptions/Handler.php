<?php

namespace App\Exceptions;

use Fomo\Request\Request;
use Fomo\Response\Response;

class Handler
{
    public function notFoundHttpException(Request $request): Response
    {
        return response()->json([
            'message' => 'not found'
        ], 404);
    }

    public function notAllowedHttpException(Request $request): Response
    {
        return response()->json([
            'message' => "this is route supported {$request->method()} method"
        ], 405);
    }
}
