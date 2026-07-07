<?php

namespace App\Http\Controllers;

use Illuminate\Http\Response;
use Illuminate\Support\Facades\Log;
use Psr\Log\LoggerInterface;

/**
 * Laravel Benchmark User Controller
 * 
 * Handles user-related endpoints for the benchmark server.
 * Follows Laravel best practices including proper error handling and logging.
 */
class UserController extends Controller
{
    /**
     * Logger instance
     *
     * @var LoggerInterface
     */
    private $logger;

    /**
     * Create a new controller instance.
     *
     * @return void
     */
    public function __construct()
    {
        $this->logger = Log::channel('benchmark');
    }

    /**
     * Create a new user
     *
     * POST /user
     *
     * @return Response Empty response for benchmarking
     */
    public function create()
    {
        $this->logger->debug('Create user endpoint accessed');
        
        return response('', Response::HTTP_OK)
            ->header('Content-Type', 'text/plain');
    }

    /**
     * Get user by ID
     *
     * GET /user/{id}
     *
     * @param string $id User identifier
     * @return Response User ID as plain text
     */
    public function show($id)
    {
        $this->logger->debug("User endpoint accessed with ID: {$id}");
        
        return response($id, Response::HTTP_OK)
            ->header('Content-Type', 'text/plain');
    }
}
