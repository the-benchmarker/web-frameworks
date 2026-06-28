<?php

namespace App\Controllers;

/**
 * User Controller for CodeIgniter4 Benchmark Server
 * 
 * Handles user-related endpoints for the benchmark server.
 * Follows CodeIgniter4 best practices including proper error handling and logging.
 */
class User extends BaseController
{
    /**
     * Create a new user
     * 
     * POST /user
     * 
     * @return string Empty response for benchmarking
     */
    public function list()
    {
        log_message('debug', 'Create user endpoint accessed');
        
        return $this->response
            ->setContentType('text/plain')
            ->setBody('')
            ->setStatusCode(200);
    }

    /**
     * Get user by ID
     * 
     * GET /user/{id}
     * 
     * @param string $id User identifier
     * @return string User ID as plain text
     */
    public function view($id)
    {
        log_message('debug', "User endpoint accessed with ID: {$id}");
        
        return $this->response
            ->setContentType('text/plain')
            ->setBody($id)
            ->setStatusCode(200);
    }
}
