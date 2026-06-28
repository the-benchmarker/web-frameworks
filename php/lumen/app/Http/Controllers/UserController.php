<?php

namespace App\Http\Controllers;

use Illuminate\Http\Response;
use Laravel\Lumen\Routing\Controller;
use Psr\Log\LoggerInterface;

/**
 * Lumen Benchmark User Controller
 * 
 * Handles user-related endpoints for the benchmark server.
 * Follows Lumen best practices including proper error handling and logging.
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
        $this->logger = app('log');
    }

    /**
     * Get user by ID
     * 
     * GET /user/{id}
     *
     * @param int $id User identifier
     * @return Response User ID as plain text
     */
    public function show(int $id)
    {
        $this->logger->debug("User endpoint accessed with ID: {$id}");
        
        return response($id, Response::HTTP_OK)
            ->header('Content-Type', 'text/plain');
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
}
