<?php

namespace App\Exceptions;

use Fomo\Request\Request;
use Fomo\Response\Response;

class Handler
{
    public function notFoundHttpException(Request $request): Response
    {
        return response()->asJson([
            'message' => 'not found'
        ], 404);
    }

    public function notAllowedHttpException(Request $request): Response
    {
        return response()->asJson([
            'message' => "this is route supported {$request->method()} method"
        ], 405);
    }
}
