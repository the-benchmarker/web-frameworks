<?php

namespace App\Http\Controllers;

use Illuminate\Http\Response;
use Illuminate\Support\Facades\Log;
use Psr\Log\LoggerInterface;

/**
 * Laravel Benchmark Application Controller
 * 
 * Handles root endpoint for the benchmark server.
 * Follows Laravel best practices including proper error handling and logging.
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
        $this->logger = Log::channel('benchmark');
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
        return response('OK', Response::HTTP_OK)
            ->header('Content-Type', 'text/plain');
    }
}
