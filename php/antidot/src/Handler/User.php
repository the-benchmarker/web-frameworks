<?php

declare(strict_types=1);

namespace App\Handler;

use Nyholm\Psr7\Response;
use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Psr\Http\Server\RequestHandlerInterface;

/**
 * User Handler for Antidot Benchmark Server
 * 
 * Handles user creation endpoint for the benchmark server.
 * Follows Antidot best practices including proper error handling and logging.
 */
class User implements RequestHandlerInterface
{
    /**
     * Handle the request
     *
     * POST /user
     *
     * @param ServerRequestInterface $request HTTP request object
     * @return ResponseInterface Empty response for benchmarking
     */
    public function handle(ServerRequestInterface $request): ResponseInterface
    {
        // Log request for benchmarking
        error_log('[' . date('Y-m-d H:i:s') . '] DEBUG - Create user endpoint accessed');
        
        return new Response(200, ['Content-Type' => 'text/plain'], '');
    }
}
