<?php

declare(strict_types=1);

namespace App\Handler;

use Nyholm\Psr7\Response;
use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Psr\Http\Server\RequestHandlerInterface;

/**
 * Home Handler for Antidot Benchmark Server
 * 
 * Handles root endpoint for the benchmark server.
 * Follows Antidot best practices including proper error handling and logging.
 */
class Home implements RequestHandlerInterface
{
    /**
     * Handle the request
     *
     * GET /
     *
     * @param ServerRequestInterface $request HTTP request object
     * @return ResponseInterface Empty response for benchmarking
     */
    public function handle(ServerRequestInterface $request): ResponseInterface
    {
        // Log request for benchmarking
        error_log('[' . date('Y-m-d H:i:s') . '] DEBUG - Root endpoint accessed');
        
        return new Response(200, ['Content-Type' => 'text/plain'], '');
    }
}
