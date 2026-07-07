<?php

namespace App\Handler;

use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Psr\Http\Server\MiddlewareInterface;
use Psr\Http\Server\RequestHandlerInterface;
use Laminas\Diactoros\Response\TextResponse;

/**
 * UserId Handler for Mezzio Benchmark Server
 * 
 * Handles user by ID endpoint for the benchmark server.
 * Follows Mezzio best practices including proper error handling and logging.
 */
class UserId implements MiddlewareInterface
{
    /**
     * Process an incoming server request.
     *
     * Processes an incoming server request in order to produce a response.
     * If unable to produce the response itself, it may delegate to the provided
     * request handler to do so.
     *
     * GET /user/{id}
     *
     * @param ServerRequestInterface $request HTTP request object
     * @param RequestHandlerInterface $handler Request handler
     * @return ResponseInterface User ID as plain text
     */
    public function process(ServerRequestInterface $request, RequestHandlerInterface $handler): ResponseInterface
    {
        $id = $request->getAttribute('id');
        
        // Log request for benchmarking
        error_log('[' . date('Y-m-d H:i:s') . '] DEBUG - User endpoint accessed with ID: ' . $id);
        
        return new TextResponse($id, 200, ['Content-Type' => ['text/plain']]);
    }
}
