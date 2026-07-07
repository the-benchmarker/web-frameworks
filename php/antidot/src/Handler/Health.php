<?php

declare(strict_types=1);

namespace App\Handler;

use Nyholm\Psr7\Response;
use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Psr\Http\Server\RequestHandlerInterface;

/**
 * Health Check Handler for Antidot Benchmark Server
 * 
 * Handles health check endpoint for the benchmark server.
 * Follows Antidot best practices including proper error handling and logging.
 */
class Health implements RequestHandlerInterface
{
    /**
     * Handle the request
     *
     * GET /health
     *
     * @param ServerRequestInterface $request HTTP request object
     * @return ResponseInterface Health status
     */
    public function handle(ServerRequestInterface $request): ResponseInterface
    {
        // Log request for benchmarking
        error_log('[' . date('Y-m-d H:i:s') . '] DEBUG - Health check endpoint accessed');
        
        return new Response(200, ['Content-Type' => 'text/plain'], 'OK');
    }
}