<?php

namespace App\Http\Controllers;

use Illuminate\Http\Response;
use Laravel\Lumen\Routing\Controller;
use Psr\Log\LoggerInterface;

/**
 * Lumen Benchmark Application Controller
 * 
 * Handles root endpoint for the benchmark server.
 * Follows Lumen best practices including proper error handling and logging.
 */
class ApplicationController extends Controller
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
     * Root endpoint handler
     * 
     * GET /
     *
     * @return Response Empty response for benchmarking
     */
    public function index()
    {
        $this->logger->debug('Root endpoint accessed');
        
        return response('', Response::HTTP_OK)
            ->header('Content-Type', 'text/plain');
    }

    /**
     * Health check endpoint for monitoring
     * 
     * GET /health
     *
     * @return Response Health status
     */
    public function healthCheck()
    {
        $this->logger->debug('Health check endpoint accessed');
        
        return response('OK', Response::HTTP_OK)
            ->header('Content-Type', 'text/plain');
    }
}
