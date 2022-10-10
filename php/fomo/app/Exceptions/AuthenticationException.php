<?php

namespace App\Exceptions;

use Exception;
use Tower\Response;

class AuthenticationException extends Exception
{
    public function handle(): Response
    {
        return response()->json([
            'message' => 'Unauthorized'
        ] , Response::HTTP_UNAUTHORIZED);
    }
}