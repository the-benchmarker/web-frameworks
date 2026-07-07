<?php

namespace App\Handler;

use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Psr\Http\Server\MiddlewareInterface;
use Psr\Http\Server\RequestHandlerInterface;
use Laminas\Diactoros\Response\TextResponse;

/**
 * Health Check Handler for Mezzio Benchmark Server
 * 
 * Handles health check endpoint for the benchmark server.
 * Follows Mezzio best practices including proper error handling and logging.
 */
class Health implements MiddlewareInterface
{
    /**
     * Process an incoming server request.
     *
     * Processes an incoming server request in order to produce a response.
     * If unable to produce the response itself, it may delegate to the provided
     * request handler to do so.
     *
     * GET /health
     *
     * @param ServerRequestInterface $request HTTP request object
     * @param RequestHandlerInterface $handler Request handler
     * @return ResponseInterface Health status
     */
    public function process(ServerRequestInterface $request, RequestHandlerInterface $handler): ResponseInterface
    {
        // Log request for benchmarking
        error_log('[' . date('Y-m-d H:i:s') . '] DEBUG - Health check endpoint accessed');
        
        return new TextResponse('OK', 200, ['Content-Type' => ['text/plain']]);
    }
}