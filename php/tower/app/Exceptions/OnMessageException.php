<?php

namespace App\Exceptions;

use Exception;
use Tower\Response;

class OnMessageException extends Exception
{
    public function __construct(string $message , string $file , string $line)
    {
        $this->message = $message;
        $this->file = $file;
        $this->line = $line;
    }

    public function handle(): Response
    {
        return response()->json([
            'message' => $this->getMessage() ,
            'file' => $this->getFile() ,
            'line' => $this->getLine() ,
        ] , Response::HTTP_INTERNAL_SERVER_ERROR);
    }
}
