<?php

namespace App\Exceptions;

use Exception;
use Tower\Response;

class MethodNotAllowedException extends Exception
{
    public function __construct(string $method)
    {
        parent::__construct($method);
    }

    public function handle(): Response
    {
        return response()->json([
            "message" => "this is route supported {$this->getMessage()} method"
        ] , Response::HTTP_METHOD_NOT_ALLOWED);
    }
}