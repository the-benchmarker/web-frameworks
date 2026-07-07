<?php

declare(strict_types=1);

namespace App\Handler;

use Nyholm\Psr7\Response;
use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Psr\Http\Server\RequestHandlerInterface;

/**
 * UserId Handler for Antidot Benchmark Server
 * 
 * Handles user by ID endpoint for the benchmark server.
 * Follows Antidot best practices including proper error handling and logging.
 */
class UserId implements RequestHandlerInterface
{
    /**
     * Handle the request
     *
     * GET /user/{id}
     *
     * @param ServerRequestInterface $request HTTP request object
     * @return ResponseInterface User ID as plain text
     */
    public function handle(ServerRequestInterface $request): ResponseInterface
    {
        $id = $request->getAttribute('id');
        
        // Log request for benchmarking
        error_log('[' . date('Y-m-d H:i:s') . '] DEBUG - User endpoint accessed with ID: ' . $id);
        
        return new Response(200, ['Content-Type' => 'text/plain'], $id);
    }
}
