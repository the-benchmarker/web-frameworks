<?php

namespace App\Handler;

use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Psr\Http\Server\MiddlewareInterface;
use Psr\Http\Server\RequestHandlerInterface;
use Laminas\Diactoros\Response\TextResponse;

/**
 * User Handler for Mezzio Benchmark Server
 * 
 * Handles user creation endpoint for the benchmark server.
 * Follows Mezzio best practices including proper error handling and logging.
 */
class User implements MiddlewareInterface
{
    /**
     * Process an incoming server request.
     *
     * Processes an incoming server request in order to produce a response.
     * If unable to produce the response itself, it may delegate to the provided
     * request handler to do so.
     *
     * POST /user
     *
     * @param ServerRequestInterface $request HTTP request object
     * @param RequestHandlerInterface $handler Request handler
     * @return ResponseInterface Empty response for benchmarking
     */
    public function process(ServerRequestInterface $request, RequestHandlerInterface $handler): ResponseInterface
    {
        // Log request for benchmarking
        error_log('[' . date('Y-m-d H:i:s') . '] DEBUG - Create user endpoint accessed');
        
        return new TextResponse('', 200, ['Content-Type' => ['text/plain']]);
    }
}
