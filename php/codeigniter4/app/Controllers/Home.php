<?php

namespace App\Controllers;

/**
 * Home Controller for CodeIgniter4 Benchmark Server
 * 
 * Handles root endpoint for the benchmark server.
 * Follows CodeIgniter4 best practices including proper error handling and logging.
 */
class Home extends BaseController
{
    /**
     * Root endpoint handler
     * 
     * GET /
     * 
     * @return string Empty response for benchmarking
     */
    public function index()
    {
        log_message('debug', 'Root endpoint accessed');
        
        return $this->response
            ->setContentType('text/plain')
            ->setBody('')
            ->setStatusCode(200);
    }

    /**
     * Health check endpoint for monitoring
     * 
     * GET /health
     * 
     * @return string Health status
     */
    public function health()
    {
        log_message('debug', 'Health check endpoint accessed');
        
        return $this->response
            ->setContentType('text/plain')
            ->setBody('OK')
            ->setStatusCode(200);
    }
}
